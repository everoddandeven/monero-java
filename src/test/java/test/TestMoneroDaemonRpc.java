package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import monero.daemon.MoneroDaemon;
import monero.daemon.model.MoneroAltChain;
import monero.daemon.model.MoneroBan;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroBlockHeader;
import monero.daemon.model.MoneroBlockTemplate;
import monero.daemon.model.MoneroCoinbaseTxSum;
import monero.daemon.model.MoneroDaemonConnection;
import monero.daemon.model.MoneroDaemonConnectionSpan;
import monero.daemon.model.MoneroDaemonInfo;
import monero.daemon.model.MoneroDaemonPeer;
import monero.daemon.model.MoneroDaemonSyncInfo;
import monero.daemon.model.MoneroDaemonUpdateCheckResult;
import monero.daemon.model.MoneroDaemonUpdateDownloadResult;
import monero.daemon.model.MoneroHardForkInfo;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroKeyImageSpentStatus;
import monero.daemon.model.MoneroMiningStatus;
import monero.daemon.model.MoneroOutput;
import monero.daemon.model.MoneroOutputDistributionEntry;
import monero.daemon.model.MoneroTx;
import monero.daemon.model.MoneroTxPoolStats;
import monero.rpc.MoneroRpcException;
import monero.utils.MoneroException;
import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletLocal;
import monero.wallet.config.MoneroSendConfig;
import utils.TestUtils;

/**
 * Tests a Monero daemon.
 */
public class TestMoneroDaemonRpc {
  
  // classes to test
  private static MoneroDaemon daemon;
  private static MoneroWallet wallet;
  
  // test configuration
  private static boolean TEST_NON_RELAYS = true;
  private static boolean TEST_RELAYS = true; // creates and relays outgoing txs
  private static boolean TEST_NOTIFICATIONS = true;
  
  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    daemon = TestUtils.getDaemonRpc();
    wallet = TestUtils.getWalletRpc();
  }
  
  @Before
  public void before() {
    
  }
  
  // -------------------------------- NON RELAYS ------------------------------
  
  @Test
  public void testIsTrusted() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.getIsTrusted();
  }
  
  @Test
  public void testGetHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int height = daemon.getHeight();
    assertTrue("Height must be greater than 0", height > 0);
  }
  
  @Test
  public void testGetBlockIdByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    assertNotNull(id);
    assertEquals(64, id.length());
  }
  
  @Test
  public void testGetBlockTemplate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.TEST_ADDRESS, 2);
    testBlockTemplate(template);
  }
  
  @Test
  public void testGetLastBlockHeader() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    testBlockHeader(lastHeader, true);
  }
  
  @Test
  public void testGetBlockHeaderById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // retrieve by id of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    MoneroBlockHeader header = daemon.getBlockHeaderById(id);
    testBlockHeader(header, true);
    assertEquals(lastHeader, header);
    
    // retrieve by id of previous to last block
    id = daemon.getBlockId(lastHeader.getHeight() - 1);
    header = daemon.getBlockHeaderById(id);
    testBlockHeader(header, true);
    assertEquals(lastHeader.getHeight() - 1, (int) header.getHeight());
  }
  
  @Test
  public void testGetBlockHeaderByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // retrieve by height of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    MoneroBlockHeader header = daemon.getBlockHeaderByHeight(lastHeader.getHeight());
    testBlockHeader(header, true);
   assertEquals(lastHeader, header);
    
    // retrieve by height of previous to last block
    header = daemon.getBlockHeaderByHeight(lastHeader.getHeight() - 1);
    testBlockHeader(header, true);
    assertEquals(lastHeader.getHeight() - 1, (int) header.getHeight());
  }
  
  // TODO: test start with no end, vice versa, inclusivity
  @Test
  public void testGetBlockHeadersByRange() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // determine start and end height based on number of blocks and how many blocks ago
    int numBlocks = 100;
    int numBlocksAgo = 100;
    int currentHeight = daemon.getHeight();
    int startHeight = currentHeight - numBlocksAgo;
    int endHeight = currentHeight - (numBlocksAgo - numBlocks) - 1;
    
    // fetch headers
    List<MoneroBlockHeader> headers = daemon.getBlockHeadersByRange(startHeight, endHeight);
    
    // test headers
    assertEquals(numBlocks, headers.size());
    for (int i = 0; i < numBlocks; i++) {
      MoneroBlockHeader header = headers.get(i);
      assertEquals(startHeight + i, (int) header.getHeight());
      testBlockHeader(header, true);
    }
  }
  
  @Test
  public void testGetBlockById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // test config
    TestContext ctx = new TestContext();
    ctx.hasHex = false;
    ctx.hasJson = true;
    ctx.headerIsFull = true;
    
    // retrieve by id of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    String id = daemon.getBlockId(lastHeader.getHeight());
    MoneroBlock block = daemon.getBlockById(id);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeader().getHeight()), block);
    assertEquals(null, block.getTxs());
    
    // retrieve by id of previous to last block
    id = daemon.getBlockId(lastHeader.getHeight() - 1);
    block = daemon.getBlockById(id);
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(lastHeader.getHeight() - 1), block);
    assertEquals(null, block.getTxs());
  }
  
  @Test
  public void testGetBlockByHeight() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // config for testing blocks
    TestContext ctx = new TestContext();
    ctx.hasHex = true;
    ctx.headerIsFull = true;
    ctx.hasTxs = false;
    
    // retrieve by height of last block
    MoneroBlockHeader lastHeader = daemon.getLastBlockHeader();
    MoneroBlock block = daemon.getBlockByHeight(lastHeader.getHeight());
    testBlock(block, ctx);
    assertEquals(daemon.getBlockByHeight(block.getHeader().getHeight()), block);
    
    // retrieve by height of previous to last block
    block = daemon.getBlockByHeight(lastHeader.getHeight() - 1);
    testBlock(block, ctx);
    assertEquals(lastHeader.getHeight() - 1, (int) block.getHeader().getHeight());
  }
  
  @Test
  public void testGetBlocksByHeightBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set number of blocks to test
    int numBlocks = 200;
    
    // select random heights  // TODO: this is horribly inefficient way of computing last 100 blocks if not shuffling
    int currentHeight = daemon.getHeight();
    List<Integer> allHeights = new ArrayList<Integer>();
    for (int i = 0; i < currentHeight - 1; i++) allHeights.add(i);
    //GenUtils.shuffle(allHeights);
    List<Integer> heights = new ArrayList<Integer>();
    for (int i = allHeights.size() - numBlocks; i < allHeights.size(); i++) heights.add(allHeights.get(i));
    
    // fetch blocks
    List<MoneroBlock> blocks = daemon.getBlocksByHeight(heights);
    
    // config for testing blocks
    // TODO: getBlocksByHeight() has inconsistent client-side pruning
    // TODO: get_blocks_by_height.bin does not return output indices (#5127)
    TestContext ctx  = new TestContext();
    ctx.hasHex = false;
    ctx.headerIsFull = false;
    ctx.hasTxs = true;
    ctx.txContext = new TestContext();
    ctx.txContext.isPruned = false;
    ctx.txContext.isConfirmed = true;
    ctx.txContext.fromGetTxPool = false;
    ctx.txContext.hasOutputIndices = false;
    ctx.txContext.fromGetBlocksByHeight = false;
    
    // test blocks
    boolean txFound = false;
    assertEquals(numBlocks, blocks.size());
    for (int i = 0; i < heights.size(); i++) {
      MoneroBlock block = blocks.get(i);
      if (!block.getTxs().isEmpty()) txFound = true;
      testBlock(block, ctx);
      assertEquals(block.getHeader().getHeight(), heights.get(i));      
    }
    assertTrue("No transactions found to test", txFound);
  }
  
  @Test
  public void testGetBlocksByIdBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    fail("Not implemented");
  }
  
  @Test
  public void testGetBlocksByRange() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get current height
    int height = daemon.getHeight();
    
    // get valid height range
    int numBlocks = 1; // TODO: RequestError: Error: read ECONNRESET or  RequestError: Error: socket hang up if > 64 or (or > 1 if test getBlocksByHeight() runs first)
    int numBlocksAgo = 190;
    assertTrue(numBlocks > 0);
    assertTrue(numBlocksAgo >= numBlocks);
    assertTrue(height - numBlocksAgo + numBlocks - 1 < height);
    int startHeight = height - numBlocksAgo;
    int endHeight = height - numBlocksAgo + numBlocks - 1;
    
    // test known start and end heights
    //console.log("Height: " + height);
    //console.log("Fecthing " + (endHeight - startHeight + 1) + " blocks [" + startHeight + ", " + endHeight + "]");
    testRange(startHeight, endHeight, height);
    
    // test unspecified start
    testRange(null, numBlocks - 1, height);
    
    // test unspecified end
    testRange(height - numBlocks - 1, null, height);
    
    // test unspecified start and end 
    //testRange(null, null, height);  // TODO: RequestError: Error: socket hang up
  };
  
  @Test
  public void testGetBlockIdsBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    //get_hashes.bin
    fail("Not implemented");
  }
  
  @Test
  public void testGetTransactionById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // context for testing txs
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = true;
    ctx.fromGetTxPool = false;
    
    // fetch each tx by id without pruning
    for (String txId : txIds) {
      MoneroTx tx = daemon.getTx(txId);
      testTx(tx, ctx);
    }
    
    // fetch each tx by id with pruning
    for (String txId : txIds) {
      MoneroTx tx = daemon.getTx(txId, true);
      ctx.isPruned = true;
      testTx(tx, ctx);
    }
    
    // fetch invalid id
    try {
      daemon.getTx("invalid tx id");
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  @Test
  public void testGetTransactionsByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // context for testing txs
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = true;
    ctx.fromGetTxPool = false;
    
    // fetch txs by id without pruning
    List<MoneroTx> txs = daemon.getTxs(txIds);
    assertEquals(txIds.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // fetch txs by id with pruning
    txs = daemon.getTxs(txIds, true);
    ctx.isPruned = true;
    assertEquals(txIds.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
    
    // fetch invalid id
    txIds.add("invalid tx id");
    try {
      daemon.getTxs(txIds);
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  @Test
  public void testGetTransactionsByIdsInPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx = getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
      txIds.add(tx.getId());
    }
    
    // fetch txs by id
    List<MoneroTx> txs = daemon.getTxs(txIds);
    
    // context for testing tx
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = false;
    ctx.fromGetTxPool = false;
    
    // test fetched txs
    assertEquals(txIds.size(), txs.size());
    for (MoneroTx tx : txs) {
      testTx(tx, ctx);
    }
  }
  
  @Test
  public void testGetTransactionHexById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // fetch each tx hex by id with and without pruning
    List<String> hexes = new ArrayList<String>();
    List<String> hexesPruned = new ArrayList<String>();
    for (String txId : txIds) {
      hexes.add(daemon.getTxHex(txId));
      hexesPruned.add(daemon.getTxHex(txId, true));
    }
    
    // test results
    assertEquals(hexes.size(), txIds.size());
    assertEquals(hexesPruned.size(), txIds.size());
    for (int i = 0; i < hexes.size(); i++) {
      assertNotNull(hexes.get(i));
      assertNotNull(hexesPruned.get(i));
      assertFalse(hexesPruned.isEmpty());
      assertTrue(hexes.get(i).length() > hexesPruned.get(i).length()); // pruned hex is shorter
    }
    
    // fetch invalid id
    try {
      daemon.getTxHex("invalid tx id");
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  @Test
  public void testGetTransactionHexesByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // fetch transaction ids to test
    List<String> txIds = getConfirmedTxIds(daemon);
    
    // fetch tx hexes by id with and without pruning
    List<String> hexes = daemon.getTxHexes(txIds);
    List<String> hexesPruned = daemon.getTxHexes(txIds, true);
    
    // test results
    assertEquals(hexes.size(), txIds.size());
    assertEquals(hexesPruned.size(), txIds.size());
    for (int i = 0; i < hexes.size(); i++) {
      assertNotNull(hexes.get(i));
      assertNotNull(hexesPruned.get(i));
      assertFalse(hexesPruned.isEmpty());
      assertTrue(hexes.get(i).length() > hexesPruned.get(i).length()); // pruned hex is shorter
    }
    
    // fetch invalid id
    txIds.add("invalid tx id");
    try {
      daemon.getTxHexes(txIds);
      throw new Error("fail");
    } catch (MoneroException e) {
      assertEquals("Invalid transaction id", e.getMessage());
    }
  }
  
  @Test
  public void testGetCoinbaseTxSum() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroCoinbaseTxSum sum = daemon.getCoinbaseTxSum(0, 50000);
    testCoinbaseTxSum(sum);
  }
  
  @Test
  public void testGetFeeEstimate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    BigInteger fee = daemon.getFeeEstimate();
    TestUtils.testUnsignedBigInteger(fee, true);
  }
  
  @Test
  public void testGetTransactionsInPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit tx to pool but don't relay
    MoneroTx tx = getUnrelayedTx(wallet, null);
    daemon.submitTxHex(tx.getHex(), true);
    
    // fetch txs in pool
    List<MoneroTx> txs = daemon.getTxPool();
    
    // context for testing tx
    TestContext ctx = new TestContext();
    ctx.isPruned = false;
    ctx.isConfirmed = false;
    ctx.fromGetTxPool = true;
    
    // test txs
    assertFalse("Test requires an unconfirmed tx in the tx pool", txs.isEmpty());
    for (MoneroTx aTx : txs) {
      testTx(aTx, ctx);
    }
    
    // flush the tx from the pool, gg
    daemon.flushTxPool(tx.getId());
  }
  
  @Test
  public void testGetIdsOfTransactionsInPoolBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    // TODO: get_transaction_pool_hashes.bin
    throw new Error("Not implemented");
  }
  
  @Test
  public void testGetTxPoolBacklogBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    // TODO: get_txpool_backlog
    throw new Error("Not implemented");
  }
  
  @Test
  public void testGetTxPoolStatisticsBin() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay (multiple txs result in binary `histo` field)
    for (int i = 0; i < 2; i++) {
      
      // submit tx hex
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
      
      // test stats
      MoneroTxPoolStats stats = daemon.getTxPoolStats();
      assertTrue(stats.getNumTxs() > i);
      testTxPoolStats(stats);
    }
  }
  
  @Test
  public void testFlushTxsFromPool() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // pool starts flushed for each test
    List<MoneroTx> txs = daemon.getTxPool();
    assertEquals(0, txs.size());
    
    // submit txs to the pool but don't relay
    for (int i = 0; i < 2; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
    }
    
    // txs are in pool
    txs = daemon.getTxPool();
    assertTrue(txs.size() >= 2);
    
    // flush tx pool
    daemon.flushTxPool();
    txs = daemon.getTxPool();
    assertTrue(txs.isEmpty());
  }
  
  @Test
  public void testFlushTxFromPoolById() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
      txs.add(tx);
    }
    
    // remove each tx from the pool by id and test
    for (int i = 0; i < txs.size(); i++) {
      
      // flush tx from pool
      daemon.flushTxPool(txs.get(i).getId());
      
      // test tx pool
      List<MoneroTx> poolTxs = daemon.getTxPool();
      assertEquals(txs.size() - i - 1, poolTxs.size());
    }
  }
  
  @Test
  public void testFlushTxsFromPoolByIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool but don't relay
    List<String> txIds = new ArrayList<String>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
      txIds.add(tx.getId());
    }
    
    // remove all txs by ids
    daemon.flushTxPool(txIds);
    
    // test tx pool
    List<MoneroTx> txs = daemon.getTxPool();
    assertEquals(0, txs.size());
  }
  
  @Test
  public void testGetSpentStatusOfKeyImages() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // submit txs to the pool to collect key images then flush
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    for (int i = 0; i < 3; i++) {
      MoneroTx tx =  getUnrelayedTx(wallet, i);
      daemon.submitTxHex(tx.getHex(), true);
      txs.add(tx);
    }
    List<String> keyImages = new ArrayList<String>();
    List<String> txIds = new ArrayList<String>();
    for (MoneroTx tx : txs) txIds.add(tx.getId());
    for (MoneroTx tx : daemon.getTxs(txIds)) {
      for (MoneroOutput vin : tx.getVins()) keyImages.add(vin.getKeyImage().getHex());
    }
    daemon.flushTxPool(txIds);
    
    // key images are not spent
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.NOT_SPENT);
    
    // submit txs to the pool but don't relay
    for (MoneroTx tx : txs) daemon.submitTxHex(tx.getHex(), true);
    
    // key images are in the tx pool
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.TX_POOL);
    
    // collect key images of confirmed txs
    keyImages = new ArrayList<String>();
    txs = getConfirmedTxs(daemon, 10);
    for (MoneroTx tx : txs) {
      for (MoneroOutput vin : tx.getVins()) keyImages.add(vin.getKeyImage().getHex());
    }
    
    // key images are all spent
    testSpentStatuses(keyImages, MoneroKeyImageSpentStatus.CONFIRMED);
  }
  
  @Test
  public void testGetOutputIndicesFromTxIdsBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new Error("Not implemented"); // get_o_indexes.bin
  }
  
  @Test
  public void testGetOutputsFromAmountsAndIndicesBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    throw new Error("Not implemented"); // get_outs.bin
  }
  
  @Test
  public void testGetOutputHistogramBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    fail("Not implemented");
//    List<MoneroOutputHistogramEntry> entries = daemon.getOutputHistogram();
//    assertFalse(entries.isEmpty());
//    for (MoneroOutputHistogramEntry entry : entries) {
//      testOutputHistogramEntry(entry);
//    }
  }
  
  @Test
  public void testGetOutputDistributionBinary() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<BigInteger> amounts = new ArrayList<BigInteger>();
    amounts.add(BigInteger.valueOf(0));
    amounts.add(BigInteger.valueOf(1));
    amounts.add(BigInteger.valueOf(10));
    amounts.add(BigInteger.valueOf(100));
    amounts.add(BigInteger.valueOf(1000));
    amounts.add(BigInteger.valueOf(10000));
    amounts.add(BigInteger.valueOf(100000));
    amounts.add(BigInteger.valueOf(1000000));
    List<MoneroOutputDistributionEntry> entries = daemon.getOutputDistribution(amounts);
    for (MoneroOutputDistributionEntry entry : entries) {
      testOutputDistributionEntry(entry);
    }
  }
  
  @Test
  public void testGetGeneralInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonInfo info = daemon.getInfo();
    testInfo(info);
  }
  
  @Test
  public void testGetSyncInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonSyncInfo syncInfo = daemon.getSyncInfo();
    testSyncInfo(syncInfo);
  }
  
  @Test
  public void testGetHardForkInformation() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroHardForkInfo hardForkInfo = daemon.getHardForkInfo();
    testHardForkInfo(hardForkInfo);
  }
  
  @Test
  public void testGetAlternativeChains() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroAltChain> altChains = daemon.getAltChains();
    for (MoneroAltChain altChain : altChains) {
      testAltChain(altChain);
    }
  }
  
  @Test
  public void testGetAlternativeBlockIds() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<String> altBlockIds = daemon.getAltBlockIds();
    for (String altBlockId : altBlockIds) {
      assertNotNull(altBlockId);
      assertEquals(64, altBlockId.length());  // TODO: common validation
    }
  }
  
  @Test
  public void testSetDownloadBandwidth() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int initVal = daemon.getDownloadLimit();
    assertTrue(initVal > 0);
    int setVal = initVal * 2;
    daemon.setDownloadLimit(setVal);
    assertEquals(setVal, daemon.getDownloadLimit());
    int resetVal = daemon.resetDownloadLimit();
    assertEquals(initVal, resetVal);
    
    // test invalid limits
    try {
      daemon.setDownloadLimit(0);
      fail("Should have thrown error on invalid input");
    } catch (MoneroException e) {
      assertEquals("Download limit must be an integer greater than 0", e.getMessage());
    }
    assertEquals(daemon.getDownloadLimit(), initVal);
  }
  
  @Test
  public void testSetUploadBandwidth() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    int initVal = daemon.getUploadLimit();
    assertTrue(initVal > 0);
    int setVal = initVal * 2;
    daemon.setUploadLimit(setVal);
    assertEquals(setVal, daemon.getUploadLimit());
    int resetVal = daemon.resetUploadLimit();
    assertEquals(initVal, resetVal);
    
    // test invalid limits
    try {
      daemon.setUploadLimit(0);
      fail("Should have thrown error on invalid input");
    } catch (MoneroException e) {
      assertEquals("Upload limit must be an integer greater than 0", e.getMessage());
    }
    assertEquals(initVal, daemon.getUploadLimit());
  }
  
  @Test
  public void testGetKnownPeers() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroDaemonPeer> peers = daemon.getKnownPeers();
    assertFalse("Daemon has no known peers to test", peers.isEmpty());
    for (MoneroDaemonPeer peer : peers) {
      testKnownPeer(peer, false);
    }
  }
  
  @Test
  public void testGetPeerConnections() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    List<MoneroDaemonConnection> connections = daemon.getConnections();
    assertFalse("Daemon has no incoming or outgoing connections to test", connections.isEmpty());
    for (MoneroDaemonConnection connection : connections) {
      testDaemonConnection(connection);
    }
  }
  
  @Test
  public void testSetOutgoingPeerLimit() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.setOutgoingPeerLimit(0);
    daemon.setOutgoingPeerLimit(8);
    daemon.setOutgoingPeerLimit(10);
  }
  
  @Test
  public void testSetIncomingPeerLimit() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    daemon.setIncomingPeerLimit(0);
    daemon.setIncomingPeerLimit(8);
    daemon.setIncomingPeerLimit(10);
  }
  
  @Test
  public void testBanPeer() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set ban
    MoneroBan ban = new MoneroBan();
    ban.setHost("192.168.1.51");
    ban.setIsBanned(true);
    ban.setSeconds((long) 60);
    daemon.setPeerBan(ban);
    
    // test ban
    List<MoneroBan> bans = daemon.getPeerBans();
    boolean found = false;
    for (MoneroBan aBan : bans) {
      testMoneroBan(aBan);
      if ("192.168.1.51".equals(aBan.getHost())) found = true;
    }
    assertTrue(found);
  }
  
  @Test
  public void testBanPeers() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // set bans
    MoneroBan ban1 = new MoneroBan();
    ban1.setHost("192.168.1.52");
    ban1.setIsBanned(true);
    ban1.setSeconds((long) 60);
    MoneroBan ban2 = new MoneroBan();
    ban2.setHost("192.168.1.53");
    ban2.setIsBanned(true);
    ban2.setSeconds((long) 60);
    List<MoneroBan> bans = new ArrayList<MoneroBan>();
    bans.add(ban1);
    bans.add(ban2);
    daemon.setPeerBans(bans);
    
    // test bans
    bans = daemon.getPeerBans();
    boolean found1 = false;
    boolean found2 = false;
    for (MoneroBan aBan : bans) {
      testMoneroBan(aBan);
      if ("192.168.1.52".equals(aBan.getHost())) found1 = true;
      if ("192.168.1.53".equals(aBan.getHost())) found2 = true;
    }
    assertTrue(found1);
    assertTrue(found2);
  }
  
  @Test
  public void testMining() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // stop mining at beginning of test
    try { daemon.stopMining(); }
    catch (MoneroException e) { }
    
    // generate address to mine to
    MoneroWallet wallet = new MoneroWalletLocal(daemon);
    String address = wallet.getPrimaryAddress();
    
    // start mining
    daemon.startMining(address, 2, false, true);
    
    // stop mining
    daemon.stopMining();
  }
  
  @Test
  public void testGetMiningStatus() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    try {
      
      // stop mining at beginning of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
      
      // test status without mining
      MoneroMiningStatus status = daemon.getMiningStatus();
      assertEquals(false, status.getIsActive());
      assertNull(status.getAddress());
      assertEquals(0, (int) status.getSpeed());
      assertEquals(0, (int) status.getNumThreads());
      assertNull(status.getIsBackground());
      
      // test status with mining
      MoneroWallet wallet = new MoneroWalletLocal(daemon);
      String address = wallet.getPrimaryAddress();
      int threadCount = 3;
      boolean isBackground = false;
      daemon.startMining(address, threadCount, isBackground, true);
      status = daemon.getMiningStatus();
      assertEquals(true, status.getIsActive());
      assertEquals(address, status.getAddress());
      assertTrue(status.getSpeed() >= 0);
      assertEquals(threadCount, (int) status.getNumThreads());
      assertEquals(isBackground, status.getIsBackground());
    } catch (MoneroException e) {
      throw e;
    } finally {
      
      // stop mining at end of test
      try { daemon.stopMining(); }
      catch (MoneroException e) { }
    }
  }
  
  @Test
  public void testSubmitMinedBlock() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // get template to mine on
    MoneroBlockTemplate template = daemon.getBlockTemplate(TestUtils.TEST_ADDRESS);
    
    // TODO monero rpc: way to get mining nonce when found in order to submit?
    
    // try to submit block hashing blob without nonce
    try {
      daemon.submitBlock(template.getBlockHashingBlob());
      fail("Should have thrown error");
    } catch (MoneroRpcException e) {
      assertEquals(-7, e.getRpcCode());
      assertEquals("Block not accepted", e.getRpcMessage());
    }
  }
  
  @Test
  public void testCheckForUpdate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    MoneroDaemonUpdateCheckResult result = daemon.checkForUpdate();
    testUpdateCheckResult(result);
  }
  
  @Test
  public void testDownloadUpdate() {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // download to default path
    MoneroDaemonUpdateDownloadResult result = daemon.downloadUpdate();
    testUpdateDownloadResult(result, null);
    
    // download to defined path
    String path = "test_download_" + +new Date().getTime() + ".tar.bz2";
    result = daemon.downloadUpdate(path);
    testUpdateDownloadResult(result, path);
    
    // test invalid path
//    try {
      result = daemon.downloadUpdate("./ohhai/there");
      fail("Should have thrown error");
//    } catch (MoneroRpcException e) {
//      assertNotEquals("Should have thrown error", e.getMessage());
//      assertEquals(500, e.getCode());  // TODO: this causes a 500, in daemon rpc?
//    }
  }
  
  // test is disabled to not interfere with other tests
  @Test
  public void testStop() throws InterruptedException {
    org.junit.Assume.assumeTrue(TEST_NON_RELAYS);
    
    // stop the daemon
    daemon.stop();
    
    // give the daemon 10 seconds to shut down
    TimeUnit.SECONDS.sleep(10);
    
    // try to interact with the daemon
    try {
      daemon.getHeight();
      throw new Error("Should have thrown error");
    } catch (MoneroException e) {
      assertNotEquals("Should have thrown error", e.getMessage());
    }
  }
  
  // ------------------------------- PRIVATE ---------------------------------
  
  /**
   * Provides context or configuration for test methods to test a type.
   */
  public static class TestContext {
    boolean hasJson;
    boolean isPruned;
    boolean isFull;
    boolean isConfirmed;
    boolean isCoinbase;
    boolean fromGetTxPool;
    boolean fromGetBlocksByHeight;
    boolean hasOutputIndices;
    boolean doNotTestCopy;
    boolean hasTxs;
    boolean hasHex;
    boolean headerIsFull;
    TestContext txContext;
    public TestContext() { }
    public TestContext(TestContext ctx) {
      throw new RuntimeException("Not implemented");
    }
  }
  
//  public static class MoneroBlockTestConfig {
//    boolean hasTxs;
//    boolean hasHex;
//    boolean headerIsFull;
//    MoneroTxCtx txConfig;
//    public MoneroBlockTestConfig(boolean hasTxs, boolean hasHex, boolean headerIsFull, MoneroTxCtx txConfig) {
//      super();
//      this.hasTxs = hasTxs;
//      this.hasHex = hasHex;
//      this.headerIsFull = headerIsFull;
//      this.txConfig = txConfig;
//    }
//  }
//  
//  public static class MoneroTxCtx {
//    boolean hasJson;
//    boolean isPruned;
//    boolean isFull;
//    boolean isConfirmed;
//    boolean isCoinbase;
//    boolean fromGetTxPool;
//    boolean fromGetBlocksByHeight;
//    boolean hasOutputIndices;
//    boolean doNotTestCopy;
//  }
//  
//  public static class MoneroOuptutCtx {
//    boolean hasJson;
//    boolean isPruned;
//    boolean isFull;
//    boolean isConfirmed;
//    boolean isCoinbase;
//    boolean fromGetTxPool;
//    boolean fromGetBlocksByHeight;
//    boolean hasOutputIndices;
//    boolean doNotTestCopy;
//  }
  
  private static void testBlockTemplate(MoneroBlockTemplate template) {
    assertNotNull(template);
    assertNotNull(template.getBlockTemplateBlob());
    assertNotNull(template.getBlockHashingBlob());
    assertNotNull(template.getDifficulty());
    assertNotNull(template.getExpectedReward());
    assertNotNull(template.getHeight());
    assertNotNull(template.getPrevId());
    assertNotNull(template.getReservedOffset());
  }
  
  private static void testBlockHeader(MoneroBlockHeader header, boolean isFull) {
    assertNotNull(header);
    assertTrue(header.getHeight() >= 0);
    assertTrue(header.getMajorVersion() >= 0);
    assertTrue(header.getMinorVersion() >= 0);
    assertTrue(header.getTimestamp() >= 0);
    assertNotNull(header.getPrevId());
    assertNotNull(header.getNonce());
    assertNull(header.getPowHash());  // never seen defined
    if (isFull) {
      assertTrue(header.getSize() > 0);
      assertTrue(header.getDepth() >= 0);
      assertTrue(header.getDifficulty().intValue() > 0);
      assertTrue(header.getCumulativeDifficulty().intValue() > 0);
      assertEquals(64, header.getId().length());
      assertTrue(header.getNumTxs() >= 0);
      assertNotNull(header.getOrphanStatus());
      assertNotNull(header.getReward());
      assertNotNull(header.getWeight());
    } else {
      assertNull(header.getSize());
      assertNull(header.getDepth());
      assertNull(header.getDifficulty());
      assertNull(header.getCumulativeDifficulty());
      assertNull(header.getId());
      assertNull(header.getNumTxs());
      assertNull(header.getOrphanStatus());
      assertNull(header.getReward());
      assertNull(header.getWeight());
    }
  }
  
  // TODO: test block deep copy
  private static void testBlock(MoneroBlock block, TestContext ctx) {
    
    // test required fields
    assertNotNull(block);
    assertFalse(block.getTxIds().isEmpty());
    testCoinbaseTx(block.getCoinbaseTx());  // TODO: coinbase tx doesn't have as much stuff, can't call testTx?
    testBlockHeader(block.getHeader(), ctx.headerIsFull);
    
    if (ctx.hasHex) {
      assertNotNull(block.getHex());
      assertTrue(block.getHex().length() > 1);
    } else {
      assertNull(block.getHex());
    }
    
    if (ctx.hasTxs) {
      assertNotNull(ctx.txContext);
      for (MoneroTx tx : block.getTxs()) {
        assertTrue(block == tx.getBlock());
        testTx(tx, ctx.txContext);
      }
    } else {
      assertNull(ctx.txContext);
      assertNull(block.getTxs());
    }
  }
  
  private static void testCoinbaseTx(MoneroTx coinbaseTx) {
    assertNotNull(coinbaseTx);
    assertNotNull(coinbaseTx.getIsCoinbase());
    assertTrue(coinbaseTx.getVersion() >= 0);
    assertNotNull(coinbaseTx.getExtra());
    assertTrue(coinbaseTx.getExtra().length > 0);
    assertTrue(coinbaseTx.getUnlockTime() >= 0);

    // TODO: coinbase tx does not have ids in binary requests so this will fail, need to derive using prunable data
    TestContext ctx = new TestContext();
    ctx.hasJson = false;
    ctx.isPruned = true;
    ctx.isFull = false;
    ctx.isConfirmed = true;
    ctx.isCoinbase = true;
    ctx.fromGetTxPool = true;
    testTx(coinbaseTx, ctx);
  }
  
  private static void testTx(MoneroTx tx, TestContext ctx) {
    
    // check inputs
    assertNotNull(tx);
    assertNotNull(ctx);
    assertNotNull(ctx.isPruned);
    assertNotNull(ctx.isConfirmed);
    assertNotNull(ctx.fromGetTxPool);
    
    // standard across all txs
    assertEquals(64, tx.getId().length());
    if (tx.getIsRelayed() == null) assertTrue(tx.getInTxPool());  // TODO monero-daemon-rpc: add relayed to get_transactions
    else assertNotNull(tx.getIsRelayed());
    assertNull(tx.getSignatures());
    assertNotNull(tx.getIsConfirmed());
    assertNotNull(tx.getInTxPool());
    assertNotNull(tx.getIsCoinbase());
    assertNotNull(tx.getIsDoubleSpend());
    assertNull(tx.getPrunableHex());
    assertTrue(tx.getVersion() >= 0);
    assertTrue(tx.getUnlockTime() >= 0);
    assertTrue(tx.getVins().size() >= 0);
    assertTrue(tx.getVouts().size() >= 0);
    for (MoneroOutput vin : tx.getVins()) assertTrue(tx == vin.getTx());
    for (MoneroOutput vout : tx.getVouts()) assertTrue(tx == vout.getTx());
    assertTrue(tx.getExtra().length > 0);
    assertNotNull(tx.getRctSignatures()); // TODO: model and return 
    if (ctx.fromGetBlocksByHeight) assertEquals(tx.getHex(), null);  // TODO: getBlocksByHeight() has inconsistent client-side pruning
    else assertFalse(tx.getHex().isEmpty());
    
    // test presence of output indices
    // TODO: change this over to vouts only
    if (tx.getIsCoinbase()) assertEquals(tx.getOutputIndices(), null); // TODO: how to get output indices for coinbase transactions?
    if (tx.getInTxPool() || ctx.fromGetTxPool || ctx.hasOutputIndices == false) assertEquals(null, tx.getOutputIndices());
    else assertFalse(tx.getOutputIndices().isEmpty());
    
    // test confirmed ctx
    if (ctx.isConfirmed == true) assertEquals(true, tx.getIsConfirmed());
    if (ctx.isConfirmed == false) assertEquals(false, tx.getIsConfirmed());
    
    // test confirmed
    if (tx.getIsConfirmed()) {
      assertNotNull(tx.getBlock());
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeader().getHeight() > 0);
      assertTrue(tx.getBlock().getTxs().contains(tx));
      assertTrue(tx.getBlock().getHeader().getHeight() > 0);
      assertTrue(tx.getBlock().getHeader().getTimestamp() > 0);
      assertEquals(true, tx.getIsRelayed());
      assertEquals(false, tx.getIsFailed());
      assertEquals(false, tx.getInTxPool());
      assertEquals(false, tx.getDoNotRelay());
      assertEquals(false, tx.getIsDoubleSpend());
      assertEquals(null, tx.getNumConfirmations()); // client must compute
    } else {
      assertEquals(null, tx.getBlock());
      assertEquals(0, (int) tx.getNumConfirmations());
    }
    
    // test in tx pool
    if (tx.getInTxPool()) {
      assertEquals(tx.getIsConfirmed(), false);
      assertEquals(tx.getIsDoubleSpend(), false);
      assertEquals(tx.getLastFailedHeight(), null);
      assertEquals(tx.getLastFailedId(), null);
      assertTrue(tx.getReceivedTimestamp() > 0);
      assertTrue(tx.getNumEstimatedBlocksUntilConfirmed() > 0);
    } else {
      assertEquals(tx.getNumEstimatedBlocksUntilConfirmed(), null);
      assertEquals(tx.getLastRelayedTimestamp(), null);
    }
    
    // test coinbase tx
    if (tx.getIsCoinbase()) {
      assertEquals(0, tx.getFee().equals(BigInteger.valueOf(0)));
      assertEquals(null, tx.getVins());
    }
    
    // test failed  // TODO: what else to test associated with failed
    if (tx.getIsFailed()) {
      assertTrue(tx.getReceivedTimestamp() > 0);
    } else {
      if (tx.getIsRelayed() == null) assertEquals(null, tx.getDoNotRelay()); // TODO monero-daemon-rpc: add relayed to get_transactions
      else if (tx.getIsRelayed()) assertEquals(false, tx.getIsDoubleSpend());
      else {
        assertEquals(false, tx.getIsRelayed());
        assertEquals(true, tx.getDoNotRelay());
        assertNotNull(tx.getIsDoubleSpend());
      }
    }
    assertEquals(tx.getLastFailedHeight(), null);
    assertEquals(tx.getLastFailedId(), null);
    
    // received time only for tx pool or failed txs
    if (tx.getReceivedTimestamp() != null) {
      assertTrue(tx.getInTxPool() || tx.getIsFailed());
    }
    
    // test relayed tx
    if (tx.getIsRelayed()) assertEquals(tx.getDoNotRelay(), false);
    if (tx.getDoNotRelay()) {
      assertTrue(!tx.getIsRelayed());
      assertTrue(!tx.getIsConfirmed());
    }
    
    // test vins and vouts
    if (!tx.getIsCoinbase()) assertFalse(tx.getVins().isEmpty());
    assertFalse(tx.getVouts().isEmpty());
    if (tx.getVins() != null) for (MoneroOutput vin : tx.getVins()) testVin(vin, ctx);
    if (tx.getVouts() != null) for (MoneroOutput vout : tx.getVouts()) testVout(vout, ctx);
    
    // test pruned vs not pruned
    if (ctx.isPruned) {
      assertEquals(null, tx.getRctSigPrunable());
      assertEquals(null, tx.getSize());
      assertEquals(null, tx.getLastRelayedTimestamp());
      assertEquals(null, tx.getReceivedTimestamp());
      assertEquals(null, tx.getPrunedHex());
      assertFalse(tx.getPrunedHex().isEmpty());
    } else {
      if (ctx.fromGetBlocksByHeight) assertEquals(null, tx.getRctSigPrunable());  // TODO: getBlocksByHeight() has inconsistent client-side pruning
      else assertNotNull(tx.getRctSigPrunable());
      assertEquals(null, tx.getPrunedHex());
      assertEquals(false, tx.getIsDoubleSpend());
      if (tx.getIsConfirmed()) {
        assertEquals(null, tx.getLastRelayedTimestamp());
        assertEquals(null, tx.getReceivedTimestamp());
      } else {
        if (tx.getIsRelayed()) assertTrue(tx.getLastRelayedTimestamp() > 0);
        else assertEquals(null, tx.getLastRelayedTimestamp());
        assertTrue(tx.getReceivedTimestamp() > 0);
      }
      assertEquals(null, tx.getPrunableHash());
      
//      if (ctx.fromGetTxPool || ctx.fromGetBlocksByHeight) assertEquals(tx.getPrunableHash(), null);  // TODO: getBlocksByHeight() has inconsistent client-side pruning
//      else assertFalse(tx.getPrunableHash().isEmpty());
//      
//      if (ctx.isPruned) assertNull(tx.getPrunableHash()); // TODO: tx may or may not have prunable hash, need to know when it's expected
//      else assertFalse(tx.getPrunableHash().isEmpty());
    }
    
    // test fields from tx pool
    if (ctx.fromGetTxPool) {
      assertTrue(tx.getSize() > 0);
      assertTrue(tx.getWeight() > 0);
      assertNotNull(tx.getIsKeptByBlock());
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedId());
      assertTrue(tx.getMaxUsedBlockHeight() >= 0);
      assertTrue(tx.getMaxUsedBlockId() > 0);
    } else {
      assertEquals(null, tx.getWeight());
      assertEquals(null, tx.getIsKeptByBlock());
      assertEquals(null ,tx.getIsFailed());
      assertEquals(null, tx.getLastFailedHeight());
      assertEquals(null, tx.getLastFailedId());
      assertEquals(null, tx.getMaxUsedBlockHeight());
      assertEquals(null, tx.getMaxUsedBlockId());
    }
    
    if (tx.getIsFailed()) {
      // TODO: implement this
    }
    
    // test deep copy
    if (!ctx.doNotTestCopy) testTxCopy(tx, ctx);
  }
  
  private static void testVin(MoneroOutput vin, TestContext ctx) {
    testOutput(vin);
    testKeyImage(vin.getKeyImage(), ctx);
    assertFalse(vin.getRingOutputIndices().isEmpty());
  }

  private static void testKeyImage(MoneroKeyImage image, TestContext ctx) {
    assertFalse(image.getHex().isEmpty());
    if (image.getSignature() != null) {
      assertNotNull(image.getSignature());
      assertFalse(image.getSignature().isEmpty());
    }
  }

  private static void testVout(MoneroOutput vout, TestContext ctx) {
    testOutput(vout);
    if (vout.getTx().getInTxPool() || ctx.hasOutputIndices == false) assertEquals(null, vout.getIndex());
    else assertTrue(vout.getIndex() >= 0);
    assertEquals(64, vout.getStealthPublicKey().length());
  }

  private static void testOutput(MoneroOutput output) {
    TestUtils.testUnsignedBigInteger(output.getAmount());
  }
  
  private static void testTxCopy(MoneroTx tx, TestContext ctx) {
    
    // copy tx and assert deep equality
    MoneroTx copy = tx.copy();
    assertTrue(copy instanceof MoneroTx);
    assertEquals(tx, copy);
    assertTrue(copy != tx);
    
    // test different vin references
    if (copy.getVins() == null) assertEquals(tx.getVins(), null);
    else {
      assertFalse(copy.getVins() == tx.getVins());
      for (int i = 0; i < copy.getVins().size(); i++) {
        if (tx.getVins().get(i).getAmount().equals(copy.getVins().get(i).getAmount())) assertTrue(tx.getVins().get(i).getAmount().equals(BigInteger.valueOf(0)));
      }
    }
    
    // test different vout references
    if (copy.getVouts() == null) assertEquals(null, tx.getVouts());
    else {
      assertTrue(copy.getVouts() != tx.getVouts());
      for (int i = 0; i < copy.getVouts().size(); i++) {
        if (tx.getVouts().get(i).getAmount() == copy.getVouts().get(i).getAmount()) assertTrue(tx.getVouts().get(i).getAmount().equals(BigInteger.valueOf(0)));
      }
    }
    
    // test copied tx
    ctx = new TestContext(ctx);
    ctx.doNotTestCopy = true; // to prevent infinite recursion
    if (tx.getBlock() != null) copy.setBlock(tx.getBlock().copy().setTxs(Arrays.asList(copy))); // copy block for testing
    testTx(copy, ctx);
    
    // test merging with copy
    MoneroTx merged = copy.merge(copy.copy());
    assertEquals(tx.toString(), merged.toString());
  }
  
  private static void testRange(Integer startHeight, Integer endHeight, Integer chainHeight) {
    int realStartHeight = startHeight == null ? 0 : startHeight;
    int realEndHeight = endHeight == null ? chainHeight - 1 : endHeight;
    List<MoneroBlock> blocks = daemon.getBlocksByRange(startHeight, endHeight);
    assertEquals(realEndHeight - realStartHeight + 1, blocks.size());
    for (int i = 0; i < blocks.size(); i++) {
      assertEquals(realStartHeight + i, (int) blocks.get(i).getHeader().getHeight());
    }
  }
  
  private static List<String> getConfirmedTxIds(MoneroDaemon daemon) {
    
    // get valid height range
    int height = daemon.getHeight();
    int numBlocks = 200;
    int numBlocksAgo = 200;
    assertTrue(numBlocks > 0);
    assertTrue(numBlocksAgo >= numBlocks);
    assertTrue(height - numBlocksAgo + numBlocks - 1 < height);
    int startHeight = height - numBlocksAgo;
    int endHeight = height - numBlocksAgo + numBlocks - 1;
    
    // get blocks
    List<MoneroBlock> blocks = daemon.getBlocksByRange(startHeight, endHeight);
    
    // collect tx ids
    List<String> txIds = new ArrayList<String>();
    for (MoneroBlock block : blocks) txIds.addAll(block.getTxIds());
    assertFalse("No transactions found in the range [" + startHeight + ", " + endHeight + "]", txIds.isEmpty());  // TODO: this fails if no txs in last 100 blocks
    return txIds;
  }
  
  private static MoneroTx getUnrelayedTx(MoneroWallet wallet, Integer accountIdx) {
    MoneroSendConfig sendConfig = new MoneroSendConfig(wallet.getPrimaryAddress(), TestUtils.MAX_FEE); 
    sendConfig.setDoNotRelay(true);
    sendConfig.setAccountIndex(accountIdx);
    MoneroTx tx = wallet.send(sendConfig);
    assertFalse(tx.getHex().isEmpty());
    assertEquals(tx.getDoNotRelay(), true);
    return tx;
  }
  
  private static void testCoinbaseTxSum(MoneroCoinbaseTxSum txSum) {
    TestUtils.testUnsignedBigInteger(txSum.getEmissionSum(), true);
    TestUtils.testUnsignedBigInteger(txSum.getFeeSum(), true);
  }
  
  private static void testTxPoolStats(MoneroTxPoolStats stats) {
    assertNotNull(stats);
    assertTrue(stats.getNumTxs() >= 0);
    if (stats.getNumTxs() > 0) {
      if (stats.getNumTxs() == 1) assertNull(stats.getHisto());
      else {
        assertNotNull(stats.getHisto());
        fail("Ready to test histogram");
      }
      assertTrue(stats.getBytesMax() > 0);
      assertTrue(stats.getBytesMed() > 0);
      assertTrue(stats.getBytesMin() > 0);
      assertTrue(stats.getBytesTotal() > 0);
      assertTrue(stats.getHisto98pc() == null || stats.getHisto98pc() > 0);
      assertTrue(stats.getOldestTimestamp() > 0);
      assertTrue(stats.getNum10m() >= 0);
      assertTrue(stats.getNumDoubleSpends() >= 0);
      assertTrue(stats.getNumFailing() >= 0);
      assertTrue(stats.getNumNotRelayed() >= 0);
    } else {
      assertNull(stats.getBytesMax());
      assertNull(stats.getBytesMed());
      assertNull(stats.getBytesMin());
      assertEquals(0, (int) stats.getBytesTotal());
      assertNull(stats.getHisto98pc());
      assertNull(stats.getOldestTimestamp());
      assertEquals(0, (int) stats.getNum10m());
      assertEquals(0, (int) stats.getNumDoubleSpends());
      assertEquals(0, (int) stats.getNumFailing());
      assertEquals(0, (int) stats.getNumNotRelayed());
      assertNull(stats.getHisto());
    }
  }
  
  // helper function to check the spent status of a key image or array of key images
  private static void testSpentStatuses(List<String> keyImages, MoneroKeyImageSpentStatus expectedStatus) {
    
    // test image
    for (String keyImage : keyImages) {
      assertEquals(expectedStatus, daemon.getSpentStatus(keyImage));
    }
    
    // test array of images
    List<MoneroKeyImageSpentStatus> statuses = daemon.getSpentStatuses(keyImages);
    assertEquals(keyImages.size(), statuses.size());
    for (MoneroKeyImageSpentStatus status : statuses) assertEquals(expectedStatus, status);
  }
  
  private static List<MoneroTx> getConfirmedTxs(MoneroDaemon daemon, int numTxs) {
    List<MoneroTx> txs = new ArrayList<MoneroTx>();
    int numBlocksPerReq = 50;
    for (int startIdx = daemon.getHeight() - numBlocksPerReq - 1; startIdx >= 0; startIdx -= numBlocksPerReq) {
      List<MoneroBlock> blocks = daemon.getBlocksByRange(startIdx, startIdx + numBlocksPerReq);
      for (MoneroBlock block : blocks) {
        if (block.getTxs() == null) continue;
        for (MoneroTx tx : block.getTxs()) {
          txs.add(tx);
          if (txs.size() == numTxs) return txs;
        }
      }
    }
    throw new RuntimeException("Could not get " + numTxs + " confirmed txs");
  }
  
  private static void testOutputDistributionEntry(MoneroOutputDistributionEntry entry) {
    TestUtils.testUnsignedBigInteger(entry.getAmount());
    assert(entry.getBase() >= 0);
    assertFalse(entry.getDistribution().isEmpty());
    assertTrue(entry.getStartHeight() >= 0);
  }
  
  private static void testInfo(MoneroDaemonInfo info) {
    assertNull(info.getVersion());
    assertTrue(info.getNumAltBlocks() >= 0);
    assertTrue(info.getBlockSizeLimit() > 0);
    assertTrue(info.getBlockSizeMedian() > 0);
    assertFalse(info.getBootstrapDaemonAddress().isEmpty());
    TestUtils.testUnsignedBigInteger(info.getCumulativeDifficulty());
    TestUtils.testUnsignedBigInteger(info.getFreeSpace());
    assertTrue(info.getNumOfflinePeers() >= 0);
    assertTrue(info.getNumOnlinePeers() >= 0);
    assertTrue(info.getHeight() >= 0);
    assertTrue(info.getHeightWithoutBootstrap() > 0);
    assertTrue(info.getNumIncomingConnections() >= 0);
    assertNotNull(info.getNetworkType());
    assertNotNull(info.getIsOffline());
    assertTrue(info.getNumOutgoingConnections() >= 0);
    assertTrue(info.getNumRpcConnections() >= 0);
    assertTrue(info.getStartTimestamp() > 0);
    assertTrue(info.getTarget() > 0);
    assertTrue(info.getTargetHeight() >= 0);
    assertFalse(info.getTopBlockId().isEmpty());
    assertTrue(info.getNumTxs() >= 0);
    assertTrue(info.getNumTxsPool() >= 0);
    assertNotNull(info.getWasBootstrapEverUsed());
    assertTrue(info.getBlockWeightLimit() > 0);
    assertTrue(info.getBlockWeightMedian() > 0);
    assertTrue(info.getDatabaseSize() > 0);
    assertNotNull(info.getUpdateAvailable());
  }

  private static void testSyncInfo(MoneroDaemonSyncInfo syncInfo) { // TODO: consistent naming, daemon in name?
    assertTrue(syncInfo instanceof MoneroDaemonSyncInfo);
    assertTrue(syncInfo.getHeight() >= 0);
    if (syncInfo.getConnections() != null) {
      assertTrue(syncInfo.getConnections().size() > 0);
      for (MoneroDaemonConnection connection : syncInfo.getConnections()) {
        testDaemonConnection(connection);
      }
    }
    if (syncInfo.getSpans() != null) {  // TODO: test that this is being hit, so far not used
      assertTrue(syncInfo.getSpans().size() > 0);
      for (MoneroDaemonConnectionSpan span : syncInfo.getSpans()) {
        testDaemonConnectionSpan(span);
      }
    }
    assertNull(syncInfo.getNextNeededPruningSeed());
    assertNull(syncInfo.getOverview());
  }

  private static void testHardForkInfo(MoneroHardForkInfo hardForkInfo) {
    assertNotNull(hardForkInfo.getEarliestHeight());
    assertNotNull(hardForkInfo.getIsEnabled());
    assertNotNull(hardForkInfo.getState());
    assertNotNull(hardForkInfo.getThreshold());
    assertNotNull(hardForkInfo.getVersion());
    assertNotNull(hardForkInfo.getNumVotes());
    assertNotNull(hardForkInfo.getVoting());
    assertNotNull(hardForkInfo.getWindow());
  }

  private static void testMoneroBan(MoneroBan ban) {
    assertNotNull(ban.getHost());
    assertNotNull(ban.getIp());
    assertNotNull(ban.getSeconds());
  }
  
  private static void testAltChain(MoneroAltChain altChain) {
    assertNotNull(altChain);
    assertFalse(altChain.getBlockIds().isEmpty());
    TestUtils.testUnsignedBigInteger(altChain.getDifficulty(), true);
    assertTrue(altChain.getHeight() > 0);
    assertTrue(altChain.getLength() > 0);
    assertEquals(64, altChain.getMainChainParentBlockId().length());
  }

  private static void testDaemonConnection(MoneroDaemonConnection connection) {
    assertTrue(connection instanceof MoneroDaemonConnection);
    testKnownPeer(connection.getPeer(), true);
    assertFalse(connection.getId().isEmpty());
    assertTrue(connection.getAvgDownload() >= 0);
    assertTrue(connection.getAvgUpload() >= 0);
    assertTrue(connection.getCurrentDownload() >= 0);
    assertTrue(connection.getCurrentUpload() >= 0);
    assertTrue(connection.getHeight() >= 0);
    assertTrue(connection.getLiveTime() >= 0);
    assertNotNull(connection.getIsLocalIp());
    assertNotNull(connection.getIsLocalHost());
    assertTrue(connection.getNumReceives() >= 0);
    assertTrue(connection.getReceiveIdleTime() >= 0);
    assertTrue(connection.getNumSends() >= 0);
    assertTrue(connection.getSendIdleTime() >= 0);
    assertNotNull(connection.getState());
    assertTrue(connection.getNumSupportFlags() >= 0); 
  }

  private static void testKnownPeer(MoneroDaemonPeer peer, boolean fromConnection) {
    assertNotNull(peer);
    assertFalse(peer.getId().isEmpty());
    assertFalse(peer.getHost().isEmpty());
    assertTrue(peer.getPort() > 0);
    assertNotNull(peer.getIsOnline());
    if (fromConnection) assertNull(peer.getLastSeenTimestamp());
    else assertTrue(peer.getLastSeenTimestamp() > 0);
    assertNull(peer.getPruningSeed());
  }

  private static void testUpdateCheckResult(MoneroDaemonUpdateCheckResult result) {
    assertTrue(result instanceof MoneroDaemonUpdateCheckResult);
    assertNotNull(result.getIsUpdateAvailable());
    if (result.getIsUpdateAvailable()) {
      assertFalse(result.getVersion().isEmpty());
      assertFalse(result.getHash().isEmpty());
      assertEquals(64, result.getHash().length());
    } else {
      assertNull(result.getVersion());
      assertNull(result.getHash());
    }
    assertNotNull("No auto uri; is daemon online?", result.getAutoUri());
    assertNotNull(result.getUserUri());
  }

  private static void testUpdateDownloadResult(MoneroDaemonUpdateDownloadResult result, String path) {
    testUpdateCheckResult(result);
    if (result.getIsUpdateAvailable()) {
      if (path != null) assertEquals(path, result.getDownloadPath());
      else assertNotNull(result.getDownloadPath());
    } else {
      assertNull(result.getDownloadPath());
    }
  }
  
  private static void testDaemonConnectionSpan(MoneroDaemonConnectionSpan span) {
    throw new RuntimeException("Not implemented");
  }
}