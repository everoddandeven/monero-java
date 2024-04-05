import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroTx;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletFull;
import monero.wallet.MoneroWalletLight;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroDestination;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTxConfig;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.model.MoneroWalletConfig;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.TestInstance;

import utils.TestUtils;
import utils.WalletEqualityUtils;
import utils.StartMining;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

@TestInstance(Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions
public class TestMoneroWalletLight extends TestMoneroWalletCommon {

  public TestMoneroWalletLight() {
      super();
  }

  @Override
  @BeforeAll
  public void beforeAll() {
    super.beforeAll();
  }
  
  @Override
  @AfterAll
  public void afterAll() {
    super.afterAll();
  }

  @Override
  protected MoneroWalletLight getTestWallet() {
      return TestUtils.getWalletLight();
  }

  public MoneroRpcConnection getRpcConnection()
  {
    return new MoneroRpcConnection(TestUtils.WALLET_LWS_URI);
  }

  @Override
  protected MoneroWalletLight openWallet(MoneroWalletConfig config) {
      // assign defaults
      if (config == null) config = new MoneroWalletConfig();
      if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
      if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
      if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(getRpcConnection());
      
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
      // assign defaults
      if (config == null) config = new MoneroWalletConfig();
      boolean random = config.getSeed() == null && config.getPrimaryAddress() == null;
      if (config.getPath() == null) config.setPath(TestUtils.TEST_WALLETS_DIR + "/" + UUID.randomUUID().toString());
      if (config.getPassword() == null) config.setPassword(TestUtils.WALLET_PASSWORD);
      if (config.getNetworkType() == null) config.setNetworkType(TestUtils.NETWORK_TYPE);
      if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(getRpcConnection());
      if (config.getRestoreHeight() == null && !random) config.setRestoreHeight(0l);
      
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
    if (config.getServer() == null && config.getConnectionManager() == null) config.setServer(getRpcConnection());
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

  // ------------------------------- BEGIN TESTS ------------------------------

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
    
    MoneroWalletLight walletKeys = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT).setNetworkType(TestUtils.NETWORK_TYPE));
    
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
      assertEquals(daemon.getHeight() - 1, walletKeys.getHeight());
      assertEquals(daemon.getHeight() - 1, walletKeys.getDaemonHeight());
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

  @Override
  protected void testSendToSingle(MoneroTxConfig config) {
    TestUtils.WALLET_TX_TRACKER.waitForWalletTxsToClearPool(wallet);
    if (config == null) config = new MoneroTxConfig();
    
    // find a primary address to send from
    boolean sufficientBalance = wallet.getBalance().compareTo(TestUtils.MAX_FEE) > 0;
    MoneroAccount fromAccount = wallet.getAccount(0);
    String fromPrimaryAddress = wallet.getUnlockedBalance().compareTo(TestUtils.MAX_FEE) > 0 ? wallet.getPrimaryAddress() : null;
    assertTrue(sufficientBalance, "No non-primary subaddress found with sufficient balance");
    assertTrue(fromPrimaryAddress != null, "Wallet is waiting on unlocked funds");
    
    // get balance before send
    BigInteger balanceBefore = wallet.getBalance();
    BigInteger unlockedBalanceBefore  = wallet.getUnlockedBalance();
    
    // init tx config
    BigInteger sendAmount = unlockedBalanceBefore.subtract(TestUtils.MAX_FEE).divide(BigInteger.valueOf(SEND_DIVISOR));
    String address = wallet.getPrimaryAddress();
    config.setDestinations(new MoneroDestination(address, sendAmount));
    config.setAccountIndex(fromAccount.getIndex());

    MoneroTxConfig configCopy = config.copy();
    
    // test sending to invalid address
    try {
      config.setAddress("my invalid address");
      if (!Boolean.FALSE.equals(config.getCanSplit())) wallet.createTxs(config);
      else wallet.createTx(config);
      fail("Should have thrown error creating tx with invalid address");
    } catch (MoneroError e) {
      assertEquals("Invalid destination address", e.getMessage());
      config.setAddress(address);
    }
    
    // send to self
    List<MoneroTxWallet> txs = wallet.createTxs(config);
    if (Boolean.FALSE.equals(config.getCanSplit())) assertEquals(1, txs.size());  // must have exactly one tx if no split
    
    // test that config is unchanged
    assertTrue(configCopy != config);
    assertEquals(configCopy, config);
    
    // test common tx set among txs
    testCommonTxSets(txs, false, false, false);
    
    // handle non-relayed transaction
    if (!Boolean.TRUE.equals(config.getRelay())) {
      
      // build test context
      TxContext ctx = new TxContext();
      ctx.wallet = wallet;
      ctx.config = config;
      ctx.isSendResponse = true;
      
      // test transactions
      for (MoneroTxWallet tx : txs) {
        testTxWallet(tx, ctx);
      }
      
      // txs are not in the pool
      for (MoneroTxWallet txCreated : txs) {
        for (MoneroTx txPool : daemon.getTxPool()) {
          assertFalse(txPool.getHash().equals(txCreated.getHash()), "Created tx should not be in the pool");
        }
      }
      
      // relay txs
      List<String> txHashes = null;
      if (!Boolean.TRUE.equals(config.getCanSplit())) txHashes = Arrays.asList(wallet.relayTx(txs.get(0))); // test relayTx() with single transaction
      else {
        List<String> txMetadatas = new ArrayList<String>();
        for (MoneroTxWallet tx : txs) txMetadatas.add(tx.getMetadata());
        txHashes = wallet.relayTxs(txMetadatas); // test relayTxs() with potentially multiple transactions
      }
      for (String txHash : txHashes) assertEquals(64, txHash.length());
      
      // fetch txs for testing
      txs = wallet.getTxs(new MoneroTxQuery().setHashes(txHashes));
    }
    
    // test that balance and unlocked balance decreased
    // TODO: test that other balances did not decrease
    // MoneroSubaddress subaddress = wallet.getSubaddress(fromAccount.getIndex(), fromSubaddress.getIndex());
    assertTrue(wallet.getBalance().compareTo(balanceBefore) < 0);
    assertTrue(wallet.getUnlockedBalance().compareTo(unlockedBalanceBefore) < 0);
    
    // query locked txs
    List<MoneroTxWallet> lockedTxs = getAndTestTxs(wallet, new MoneroTxQuery().setIsLocked(true), null, true);
    for (MoneroTxWallet lockedTx : lockedTxs) assertEquals(true, lockedTx.isLocked());
    
    // build test context
    TxContext ctx = new TxContext();
    ctx.wallet = wallet;
    ctx.config = config;
    ctx.isSendResponse = Boolean.TRUE.equals(config.getRelay());
    
    // test transactions
    assertTrue(txs.size() > 0);
    for (MoneroTxWallet tx : txs) {
      testTxWallet(tx, ctx);
      assertEquals(fromAccount.getIndex(), tx.getOutgoingTransfer().getAccountIndex());
      assertEquals(0, tx.getOutgoingTransfer().getSubaddressIndices().size());
      assertTrue(sendAmount.equals(tx.getOutgoingAmount()));
      if (config.getPaymentId() != null) assertEquals(config.getPaymentId(), tx.getPaymentId());
      
      // test outgoing destinations
      if (tx.getOutgoingTransfer() != null && tx.getOutgoingTransfer().getDestinations() != null) {
        assertEquals(1, tx.getOutgoingTransfer().getDestinations().size());
        for (MoneroDestination destination : tx.getOutgoingTransfer().getDestinations()) {
          testDestination(destination);
          assertEquals(destination.getAddress(), address);
          assertTrue(sendAmount.equals(destination.getAmount()));
        }
      }
      
      // tx is among locked txs
      boolean found = false;
      for (MoneroTxWallet locked : lockedTxs) {
        if (locked.getHash().equals(tx.getHash())) {
          found = true;
          break;
        }
      }
      assertTrue(found, "Created txs should be among locked txs");
    }
    
    // if tx was relayed in separate step, all wallets will need to wait for tx to confirm in order to reliably sync
    if (!Boolean.TRUE.equals(config.getRelay())) {
      TestUtils.WALLET_TX_TRACKER.reset(); // TODO: resetExcept(wallet), or does this test wallet also need to be waited on?
    }
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
    MoneroWalletFull offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l).setNetworkType(TestUtils.NETWORK_TYPE));
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      closeWallet(offlineWallet);
    }
  };

  // Supports view-only and offline wallets to create, sign, and submit transactions
  @SuppressWarnings("unused")
  @Test
  @Override
  public void testViewOnlyAndOfflineWallets() {
    assumeTrue(!LITE_MODE && (TEST_NON_RELAYS || TEST_RELAYS));
    
    // create view-only and offline wallets
    MoneroWallet viewOnlyWallet = createWallet(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setRestoreHeight(TestUtils.FIRST_RECEIVE_HEIGHT));
    MoneroWallet offlineWallet = createWalletFull(new MoneroWalletConfig().setPrimaryAddress(wallet.getPrimaryAddress()).setPrivateViewKey(wallet.getPrivateViewKey()).setPrivateSpendKey(wallet.getPrivateSpendKey()).setServerUri(TestUtils.OFFLINE_SERVER_URI).setRestoreHeight(0l));
    assertFalse(offlineWallet.isConnectedToDaemon());
    viewOnlyWallet.sync();
    
    // test tx signing with wallets
    try {
      testViewOnlyAndOfflineWallets(viewOnlyWallet, offlineWallet);
    } finally {
      closeWallet(viewOnlyWallet);
      closeWallet(offlineWallet);
    }
  }

  // Can re-sync an existing wallet from scratch
  @Test
  public void testResyncExisting() {
    assertTrue(MoneroWalletFull.walletExists(TestUtils.WALLET_FULL_PATH));
    MoneroWalletLight wallet = createWallet();
    wallet.setDaemonConnection(getRpcConnection());
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
    assertEquals(daemon.getHeight() - 1, wallet.getHeight());
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

  // Has correct accounting across accounts, subaddresses, txs, transfers, and outputs
  @Test
  @Override
  public void testAccounting() {
    assumeTrue(TEST_NON_RELAYS);
    
    // pre-fetch wallet balances, accounts, subaddresses, and txs
    BigInteger walletBalance = wallet.getBalance();
    BigInteger walletUnlockedBalance = wallet.getUnlockedBalance();
    List<MoneroAccount> accounts = wallet.getAccounts(true);  // includes subaddresses
    
    // test wallet balance
    TestUtils.testUnsignedBigInteger(walletBalance);
    TestUtils.testUnsignedBigInteger(walletUnlockedBalance);
    assertTrue(walletBalance.compareTo(walletUnlockedBalance) >= 0);
    
    // test that wallet balance equals sum of account balances
    BigInteger accountsBalance = BigInteger.valueOf(0);
    BigInteger accountsUnlockedBalance = BigInteger.valueOf(0);
    for (MoneroAccount account : accounts) {
      testAccount(account); // test that account balance equals sum of subaddress balances
      accountsBalance = accountsBalance.add(account.getBalance());
      accountsUnlockedBalance = accountsUnlockedBalance.add(account.getUnlockedBalance());
    }
    assertEquals(0, walletBalance.compareTo(accountsBalance));
    assertEquals(0, walletUnlockedBalance.compareTo(accountsUnlockedBalance));
    
//    // test that wallet balance equals net of wallet's incoming and outgoing tx amounts
//    // TODO monero-wallet-rpc: these tests are disabled because incoming transfers are not returned when sent from the same account, so doesn't balance #4500
//    // TODO: test unlocked balance based on txs, requires e.g. tx.isLocked()
//    // TODO: update this test with new api
//    BigInteger outgoingSum = BigInteger.valueOf(0);
//    BigInteger incomingSum = BigInteger.valueOf(0);
//    for (MoneroTxWallet tx : txs) {
//      if (tx.getOutgoingAmount() != null) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//      if (tx.getIncomingAmount() != null) incomingSum = incomingSum.add(tx.getIncomingAmount());
//    }
//    assertEquals(walletBalance, incomingSum.subtract(outgoingSum));
//
//    // test that each account's balance equals net of account's incoming and outgoing tx amounts
//    for (MoneroAccount account : accounts) {
//      if (account.getIndex() != 1) continue; // find 1
//      outgoingSum = BigInteger.valueOf(0);
//      incomingSum = BigInteger.valueOf(0);
//      let filter = new MoneroTxFilter();
//      filter.setAccountIndex(account.getIndex());
//      for (let tx of txs.filter(tx => filter.meetsCriteria(tx))) { // normally we'd call wallet.getTxs(filter) but we're using pre-fetched txs
//        if (tx.getId() === "8d3919d98dd5a734da8c52eddc558db3fbf059ad55d432f0052ecd59ef122ecb") console.log(tx.toString(0));
//
//        //console.log((tx.getOutgoingAmount() ? tx.getOutgoingAmount().toString() : "") + ", " + (tx.getIncomingAmount() ? tx.getIncomingAmount().toString() : ""));
//        if (tx.getOutgoingAmount()) outgoingSum = outgoingSum.add(tx.getOutgoingAmount());
//        if (tx.getIncomingAmount()) incomingSum = incomingSum.add(tx.getIncomingAmount());
//      }
//      assertEquals(incomingSum.subtract(outgoingSum).toString(), account.getBalance().toString());
//    }
    
    // balance may not equal sum of unspent outputs if unconfirmed txs
    // TODO monero-wallet-rpc: reason not to return unspent outputs on unconfirmed txs? then this isn't necessary
    List<MoneroTxWallet> txs = wallet.getTxs();
    boolean hasUnconfirmedTx = false;
    for (MoneroTxWallet tx : txs) if (tx.inTxPool()) hasUnconfirmedTx = true;
    
    // wallet balance is sum of all unspent outputs
    BigInteger walletSum = BigInteger.valueOf(0);
    for (MoneroOutputWallet output : wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false))) walletSum = walletSum.add(output.getAmount());
    if (!walletBalance.equals(walletSum)) {
      
      // txs may have changed in between calls so retry test
      walletSum = BigInteger.valueOf(0);
      for (MoneroOutputWallet output : wallet.getOutputs(new MoneroOutputQuery().setIsSpent(false))) walletSum = walletSum.add(output.getAmount());
      if (!walletBalance.equals(walletSum)) assertTrue(hasUnconfirmedTx, "Wallet balance must equal sum of unspent outputs if no unconfirmed txs");
    }
    
    // account balances are sum of their unspent outputs
    for (MoneroAccount account : accounts) {
      BigInteger accountSum = BigInteger.valueOf(0);
      List<MoneroOutputWallet> accountOutputs = wallet.getOutputs(new MoneroOutputQuery().setAccountIndex(account.getIndex()).setIsSpent(false));
      for (MoneroOutputWallet output : accountOutputs) accountSum = accountSum.add(output.getAmount());
      if (!account.getBalance().equals(accountSum)) assertTrue(hasUnconfirmedTx, "Account balance must equal sum of its unspent outputs if no unconfirmed txs");
    }
  }
  

  @Override
  protected void testAccount(MoneroAccount account) {
    
    // test account
    assertNotNull(account);
    assertTrue(account.getIndex() >= 0);
    MoneroUtils.validateAddress(account.getPrimaryAddress(), TestUtils.NETWORK_TYPE);
    TestUtils.testUnsignedBigInteger(account.getBalance());
    TestUtils.testUnsignedBigInteger(account.getUnlockedBalance());
    
    // if given, test subaddresses and that their balances add up to account balances
    if (account.getSubaddresses() != null) {
      for (int i = 0; i < account.getSubaddresses().size(); i++) {
        testSubaddress(account.getSubaddresses().get(i));
        assertEquals(account.getIndex(), account.getSubaddresses().get(i).getAccountIndex());
        assertEquals(i, (int) account.getSubaddresses().get(i).getIndex());
      }
    }
    
    // tag must be undefined or non-empty
    String tag = account.getTag();
    assertTrue(tag == null || tag.length() > 0);
  }

  @Override
  protected void testSubaddress(MoneroSubaddress subaddress) {
    assertTrue(subaddress.getAccountIndex() >= 0);
    assertTrue(subaddress.getIndex() >= 0);
    assertNotNull(subaddress.getAddress());
    assertTrue(subaddress.getLabel() == null || !subaddress.getLabel().isEmpty());
    TestUtils.testUnsignedBigInteger(subaddress.getBalance());
    TestUtils.testUnsignedBigInteger(subaddress.getUnlockedBalance());
    assertTrue(subaddress.getNumUnspentOutputs() >= 0);
    assertNotNull(subaddress.isUsed());
    if (subaddress.getBalance().compareTo(BigInteger.valueOf(0)) > 0) assertTrue(subaddress.isUsed());
    assertTrue(subaddress.getNumBlocksToUnlock() >= 0);
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
