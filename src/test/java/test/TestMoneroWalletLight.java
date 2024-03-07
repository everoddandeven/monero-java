package test;

import monero.common.MoneroError;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletLight;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroWalletConfig;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import utils.TestUtils;
import utils.WalletEqualityUtils;
import utils.StartMining;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions

public class TestMoneroWalletLight extends TestMoneroWalletCommon {

    public TestMoneroWalletLight() {
        super();
    }

    @Override
    protected MoneroWalletLight getTestWallet() {
        return TestUtils.getWalletLight();
    }

    @Override
    protected MoneroWalletLight openWallet(MoneroWalletConfig config) {
        return MoneroWalletLight.openWallet(config);
    }

    /**
     * Create a test wallet with default configuration for each wallet type.
     *
     * @param config configures the wallet to create
     * @return MoneroWallet is the created wallet
     */
    @Override
    protected MoneroWalletLight createWallet(MoneroWalletConfig config) {
        return MoneroWalletLight.createWallet(config);
    }

    protected MoneroWalletLight createWallet() {
        return MoneroWalletLight.createWallet(TestUtils.getWalletLightConfig());
    }

  protected MoneroWalletFull createWalletFull(MoneroWalletConfig config) {
    return createWalletFull(config, true);
  }
  
  protected MoneroWalletFull createWalletFull(MoneroWalletConfig config, boolean startSyncing) {
    
    // assign defaults
    if (config == null) config = new MoneroWalletConfig();
    boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
    if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
    if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
    if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(daemon.getRpcConnection());
    if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
    
    // create wallet
    MoneroWalletFull wallet = MoneroWalletFull.createWallet(config);
    if (!random) assertEquals(config.getRestoreHeight() == null ? 0l : config.getRestoreHeight(), wallet.getRestoreHeight());
    if (startSyncing != false && wallet.isConnectedToDaemon()) wallet.startSyncing(TestUtils.SYNC_PERIOD_IN_MS);
    return wallet;
  }

    @Override
    protected void closeWallet(MoneroWallet wallet, boolean save) {
        wallet.close(save);
    }

    /**
     * Get the wallet's supported languages for the seed.  This is an
     * instance method for wallet rpc and a static utility for other wallets.
     *
     * @return List<String> are the wallet's supported languages
     */
    @Override
    protected List<String> getSeedLanguages() {
        return MoneroWalletLight.getSeedLanguages();
    }

  /**
   * Test the daemon's ability to not hang from wallets which are continuously
   * syncing, have registered listeners, and which are not closed.
   */
  @Test
  public void testCreateWalletsWithoutClose() {
    
    // lets make some wallets and then go away
    for (int i = 0; i < 20; i++) {
      MoneroWalletLight willLeaveYouHanging = createWallet();
      willLeaveYouHanging.startSyncing();
      //willLeaveYouHanging.addListener(new MoneroWalletListener());  // listen for wallet events which could aggrevate hanging
    }
    
    // check in on the daemon
    daemon.getHeight();
    
    // start mining
    try { StartMining.startMining(); }
    catch (MoneroError e) { }
    
    // wait for a block
    daemon.waitForNextBlockHeader();
    
    // stop mining
    try { daemon.stopMining(); }
    catch (MoneroError e) { }
    
    // check in on the daemon
    daemon.getHeight();
    
    // wallet's intentionally not closed (daemon da man)
  }
  
  // ------------------------------- BEGIN TESTS ------------------------------

    // Can get the daemon's height
    @Test
    public void testDaemon() {
        assumeTrue(TEST_NON_RELAYS);
        assertTrue(wallet.isConnectedToDaemon());
        long daemonHeight = wallet.getDaemonHeight();
        assertTrue(daemonHeight > 0);
    }

    // Can create a light wallet from keys
  @Test
  public void testCreateWalletFromKeys() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    
    MoneroWalletLight walletKeys = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    
    try {
      assertEquals(wallet.getPrimaryAddress(), walletKeys.getPrimaryAddress());
      assertEquals(wallet.getPrivateViewKey(), walletKeys.getPrivateViewKey());
      assertEquals(wallet.getPrivateSpendKey(), walletKeys.getPrivateSpendKey());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletKeys.getRestoreHeight());
      assertTrue(walletKeys.isConnectedToDaemon());
      assertFalse(walletKeys.isSynced());
    } finally {
      walletKeys.close();
    }
  }

  // Can sync a wallet created from keys
  @Test
  public void testSyncWalletFromKeys() {
    assumeTrue(TEST_NON_RELAYS);
    
    // recreate test wallet from keys
    MoneroWalletLight walletKeys = getTestWallet();
    // create ground truth wallet for comparison
    MoneroWalletFull walletGt = TestUtils.createWalletGroundTruth(TestUtils.NETWORK_TYPE, TestUtils.SEED, null, TestUtils.FIRST_RECEIVE_HEIGHT);
    
    // test wallet and close as final step
    try {
      assertEquals(walletKeys.getSeed(), walletGt.getSeed());
      assertEquals(walletKeys.getPrimaryAddress(), walletGt.getPrimaryAddress());
      assertEquals(walletKeys.getPrivateViewKey(), walletGt.getPrivateViewKey());
      assertEquals(walletKeys.getPublicViewKey(), walletGt.getPublicViewKey());
      assertEquals(walletKeys.getPrivateSpendKey(), walletGt.getPrivateSpendKey());
      assertEquals(walletKeys.getPublicSpendKey(), walletGt.getPublicSpendKey());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, walletGt.getRestoreHeight());
      assertTrue(walletKeys.isConnectedToDaemon());
      assertFalse(walletKeys.isSynced());
      
      // sync the wallet
      //TestMoneroWalletFull.SyncProgressTester progressTester = new TestMoneroWalletFull.SyncProgressTester(walletKeys, TestUtils.FIRST_RECEIVE_HEIGHT, walletKeys.getDaemonMaxPeerHeight());
      //MoneroSyncResult result = walletKeys.sync(progressTester);
      //progressTester.onDone(walletKeys.getDaemonHeight());

      MoneroSyncResult result = walletKeys.sync();

      // test result after syncing
      assertTrue(walletKeys.isSynced());
      assertEquals(walletKeys.getDaemonHeight() - TestUtils.FIRST_RECEIVE_HEIGHT, (long) result.getNumBlocksFetched());
      assertTrue(result.getReceivedMoney());
      assertEquals(daemon.getHeight(), walletKeys.getHeight());
      assertEquals(daemon.getHeight(), walletKeys.getDaemonHeight());
      assertEquals(TestUtils.FIRST_RECEIVE_HEIGHT, (long) walletKeys.getTxs().get(0).getHeight());  // wallet should be fully synced so first tx happens on true restore height
      
      // compare with ground truth
      //testWalletEqualityOnChain(walletGt, walletKeys);
    } finally {
      walletGt.close(true);
      walletKeys.close();
    }
    
    // TODO monero-project: importing key images can cause erasure of incoming transfers per wallet2.cpp:11957 which causes this test to fail
//  // sync the wallets until same height
//  while (wallet.getHeight() != walletKeys.getHeight()) {
//    wallet.sync();
//    walletKeys.sync(new WalletSyncPrinter());
//  }
//
//  List<MoneroKeyImage> keyImages = walletKeys.exportKeyImages();
//  walletKeys.importKeyImages(keyImages);
  }


  // Is compatible with monero-wallet-rpc outputs and offline transaction signing
  @SuppressWarnings("unused")
  @Test
  public void testViewOnlyAndOfflineWalletCompatibility() throws InterruptedException, IOException {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only wallet in wallet rpc process
    MoneroWalletLight viewOnlyWallet = TestUtils.getWalletLight();
    viewOnlyWallet.sync();
    
    // create offline full wallet
    MoneroWalletFull offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      closeWallet(offlineWallet);
    }
  };

  // Can re-sync an existing wallet from scratch
  @Test
  public void testResyncExisting() {
    assertTrue(MoneroWalletFull.walletExists(TestUtils.WALLET_FULL_PATH));
    MoneroWalletLight wallet = createWallet();
    wallet.setDaemonConnection(TestUtils.getDaemonRpc().getRpcConnection());
    //long startHeight = TestUtils.TEST_RESTORE_HEIGHT;
    long startHeight = 0;
    //SyncProgressTester progressTester = new SyncProgressTester(wallet, startHeight, wallet.getDaemonHeight());
    wallet.setRestoreHeight(1);
    //MoneroSyncResult result = wallet.sync(1l, progressTester);
    MoneroSyncResult result = wallet.sync(1l);
    //progressTester.onDone(wallet.getDaemonHeight());
    
    // test result after syncing
    assertTrue(wallet.isConnectedToDaemon());
    assertTrue(wallet.isSynced());
    assertEquals(wallet.getDaemonHeight() - startHeight, (long) result.getNumBlocksFetched());
    assertTrue(result.getReceivedMoney());
    assertEquals(daemon.getHeight(), wallet.getHeight());
    wallet.close();
  }

  // Can be closed
  @Test
  public void testClose() {
    assumeTrue(TEST_NON_RELAYS);
    
    // create a test wallet
    MoneroWalletLight wallet = createWallet();
    wallet.sync();
    assertTrue(wallet.getHeight() > 1);
    assertTrue(wallet.isSynced());
    assertFalse(wallet.isClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.isClosed());
    
    // attempt to interact with the wallet
    try { wallet.getHeight(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.getSeed(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.sync(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.startSyncing(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    try { wallet.stopSyncing(); }
    catch (MoneroError e) { assertEquals("Wallet is closed", e.getMessage()); }
    
    // re-open the wallet
    wallet = createWallet();
    wallet.sync();
    assertEquals(wallet.getDaemonHeight(), wallet.getHeight());
    assertFalse(wallet.isClosed());
    
    // close the wallet
    wallet.close();
    assertTrue(wallet.isClosed());
  }
  
    // Does not leak memory ?
  @Test
  @Disabled
  public void testMemoryLeak() {
    System.out.println("Infinite loop starting, monitor memory in OS process manager...");
    System.gc();
    int i = 0;
    boolean closeWallet = false;
    long time = System.currentTimeMillis();
    if (closeWallet) wallet.close(true);
    while (true) {
      if (closeWallet) wallet = TestUtils.getWalletLight();
      wallet.sync();
      wallet.getTxs();
      wallet.getTransfers();
      wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false));
      if (i % 100 == 0) {
        System.out.println("Garbage collecting on iteration " + i);
        System.gc();
        System.out.println((System.currentTimeMillis() - time) + " ms since last GC");
        time = System.currentTimeMillis();
      }
      if (closeWallet) wallet.close(true);
      i++;
    }
  }

  // possible configuration: on chain xor local wallet data ("strict"), txs ordered same way? TBD
  private static void testWalletEqualityOnChain(MoneroWalletFull wallet1, MoneroWalletLight wallet2) {
    WalletEqualityUtils.testWalletEqualityOnChain(wallet1, wallet2);
    assertEquals(wallet1.getNetworkType(), wallet2.getNetworkType());
    assertEquals(wallet1.getRestoreHeight(), wallet2.getRestoreHeight());
    assertEquals(wallet1.getDaemonConnection(), wallet2.getDaemonConnection());
    assertEquals(wallet1.getSeedLanguage(), wallet2.getSeedLanguage());
    // TODO: more jni-specific extensions
  }
  
}
