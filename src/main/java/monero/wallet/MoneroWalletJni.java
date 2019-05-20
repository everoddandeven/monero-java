package monero.wallet;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.rpc.MoneroRpcConnection;
import monero.utils.MoneroException;
import monero.wallet.model.MoneroAccount;
import monero.wallet.model.MoneroAccountTag;
import monero.wallet.model.MoneroAddressBookEntry;
import monero.wallet.model.MoneroCheckReserve;
import monero.wallet.model.MoneroCheckTx;
import monero.wallet.model.MoneroIntegratedAddress;
import monero.wallet.model.MoneroKeyImageImportResult;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroSubaddress;
import monero.wallet.model.MoneroSyncListener;
import monero.wallet.model.MoneroSyncResult;
import monero.wallet.model.MoneroTransfer;
import monero.wallet.model.MoneroTxWallet;
import monero.wallet.request.MoneroOutputRequest;
import monero.wallet.request.MoneroSendRequest;
import monero.wallet.request.MoneroTransferRequest;
import monero.wallet.request.MoneroTxRequest;

/**
 * Implements a Monero wallet using JNI to bridge to Monero core c++.
 */
public class MoneroWalletJni extends MoneroWalletDefault {
  
  // load Monero core c++ as a dynamic library
  static {
    System.loadLibrary("monero-java");
  }
  
  // ----------------------------- PUBLIC STATIC ------------------------------
  
  /**
   * Indicates if the wallet at the given path exists.
   * 
   * @param path is the path to check for existence
   * @return true if a wallet exists at the given path, false otherwise
   */
  public static boolean walletExists(String path) {
    return walletExistsJni(path);
  }
  
  // --------------------------------- INSTANCE -------------------------------
  
  // instance variables
  private long walletHandle;    // memory address of corresponding wallet in c++; this variable is read directly by name in c++
  private long listenerHandle;  // memory address of corresponding listener in c++; this variable is read directly by name in c++
  
  // private static variables
  private static String DEFAULT_LANGUAGE = "English";
  
  /**
   * Construct a wallet with a randomly generated seed.
   */
  public MoneroWalletJni() {
    this(MoneroNetworkType.MAINNET, null, null);
  }
  
  /**
   * Construct a wallet with a randomly generated seed.
   * 
   * @param networkType is the wallet's network type (default = MoneroNetworkType.MAINNET)
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param language is the wallet and mnemonic's language (default = "English")
   */
  public MoneroWalletJni(MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, String language) {
    if (networkType == null) networkType = MoneroNetworkType.MAINNET;
    if (language == null) language = DEFAULT_LANGUAGE;
    if (daemonConnection == null) this.walletHandle = createWalletRandomJni(networkType.ordinal(), null, null, null, language);
    else this.walletHandle = createWalletRandomJni(networkType.ordinal(), daemonConnection.getUri(), daemonConnection.getUsername(), daemonConnection.getPassword(), language);
    this.setListenerJni(new WalletListenerJniImpl());
  }
  
  /**
   * Construct a wallet from a mnemonic phrase.
   * 
   * @param mnemonic is the mnemonic of the wallet to construct
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   */
  public MoneroWalletJni(String mnemonic, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, Integer restoreHeight) {
    if (networkType == null) throw new MoneroException("Must provide a network type");
    if (restoreHeight == null) restoreHeight = 0;
    this.walletHandle = createWalletFromMnemonicJni(mnemonic, networkType.ordinal(), restoreHeight);
    if (daemonConnection != null) setDaemonConnection(daemonConnection);
  }

  /**
   * Construct a wallet from an address, view key, and spend key.
   * 
   * @param address is the address of the wallet to construct
   * @param viewKey is the view key of the wallet to construct
   * @param spendKey is the spend key of the wallet to construct
   * @param networkType is the wallet's network type
   * @param daemonConnection is connection information to a daemon (default = an unconnected wallet)
   * @param restoreHeight is the block height to restore (i.e. scan the chain) from (default = 0)
   * @param language is the wallet and mnemonic's language (default = "English")
   */
  public MoneroWalletJni(String address, String viewKey, String spendKey, MoneroNetworkType networkType, MoneroRpcConnection daemonConnection, Integer restoreHeight, String language) {
    if (restoreHeight == null) restoreHeight = 0;
    if (networkType == null) throw new MoneroException("Must provide a network type");
    if (language == null) language = DEFAULT_LANGUAGE;
    this.walletHandle = createWalletFromKeysJni(address, viewKey, spendKey, networkType.ordinal(), restoreHeight, language);
    if (daemonConnection != null) setDaemonConnection(daemonConnection);
    this.listenerHandle = setListenerJni(new WalletListenerJniImpl());
  }
  
  /**
   * Construct a wallet by opening a wallet file on disk.
   * 
   * @param path is the path to the wallet file to open
   * @param password is the password of the wallet file to open
   * @param networkType is the wallet's network type
   */
  public MoneroWalletJni(String path, String password, MoneroNetworkType networkType) {
    if (!walletExistsJni(path)) throw new MoneroException("Wallet does not exist: " + path);
    if (networkType == null) throw new MoneroException("Must provide a network type");
    this.walletHandle = openWalletJni(path, password, networkType.ordinal());
  }
  
  // ------------ WALLET METHODS SPECIFIC TO JNI IMPLEMENTATION ---------------
  
  public void setDaemonConnection(MoneroRpcConnection daemonConnection) {
    if (daemonConnection == null) setDaemonConnectionJni("", "", "");
    else setDaemonConnectionJni(daemonConnection.getUri().toString(), daemonConnection.getUsername(), daemonConnection.getPassword());
  }
  
  public MoneroRpcConnection getDaemonConnection() {
    String[] vals = getDaemonConnectionJni();
    return vals[0] == null ? null : new MoneroRpcConnection(vals[0], vals[1], vals[2]);
  }
  
  // TODO: comments and other jni specific methods
  
  public String getPath() {
    String path = getPathJni();
    return path.isEmpty() ? null : path;
  }
  
  public MoneroNetworkType getNetworkType() {
    return MoneroNetworkType.values()[getNetworkTypeJni()];
  }
  
  public String getLanguage() {
    return getLanguageJni();
  }
  
  public int getRestoreHeight() {
    return (int) getRestoreHeightJni();
  }
  
  // TODO: can set restore height, start refresh, pause refresh, isSynchronized()
  public void pauseSync() {
    throw new RuntimeException("Not implemented");
  }
  
  // TODO: createFromJson? don't automatically create file?
  public String toJson() {
    throw new RuntimeException("Not implemented");
  }
  
  public void save(String path, String password) {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Re-save the wallet at its current path.
   */
  public void save() {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Close the wallet.
   */
  public void close() {
    throw new RuntimeException("Not implemented");
  }
  
  // -------------------------- COMMON WALLET METHODS -------------------------

  @Override
  public String getSeed() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getMnemonic() {
    return getMnemonicJni();
  }

  @Override
  public String getPublicViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getPrivateViewKey() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getLanguages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int getHeight() {
    return (int) getHeightJni();  // TODO: switch heights to longs
  }

  @Override
  public int getChainHeight() {
    return (int) getChainHeightJni();  // TODO: switch heights to longs
  }

  @Override
  public MoneroIntegratedAddress getIntegratedAddress(String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSyncResult sync(Integer startHeight, Integer endHeight, MoneroSyncListener listener) {
    if (endHeight != null) throw new MoneroException("Monero core wallet does not support syncing to an end height");
    //if (listener != null) throw new RuntimeException("sync listening not yet implemented");
    
    if (startHeight == null) startHeight = Math.max(getHeight(), getRestoreHeight());
    syncJni(startHeight);
    return null;
    //throw new RuntimeException("Done syncing but need to return sync results");
  }

  @Override
  public void rescanBlockchain() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean isMultisigImportNeeded() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroAccount createAccount(String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSubaddress createSubaddress(int accountIdx, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getAddress(int accountIdx, int subaddressIdx) {
    return getAddressJni(accountIdx, subaddressIdx);
  }

  @Override
  public MoneroSubaddress getAddressIndex(String address) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public BigInteger getUnlockedBalance(int accountIdx, int subaddressIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> getTxs(MoneroTxRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTransfer> getTransfers(MoneroTransferRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroOutputWallet> getOutputs(MoneroOutputRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getKeyImages() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxWallet send(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sendSplit(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroTxWallet sweepOutput(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sweepAllUnlocked(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroTxWallet> sweepDust(boolean doNotRelay) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> relayTxs(Collection<String> txMetadatas) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<String> getTxNotes(Collection<String> txIds) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setTxNotes(Collection<String> txIds, Collection<String> notes) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String sign(String msg) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean verify(String msg, String address, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxKey(String txId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckTx checkTxKey(String txId, String txKey, String address) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getTxProof(String txId, String address, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckTx checkTxProof(String txId, String address, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getSpendProof(String txId, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public boolean checkSpendProof(String txId, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProofWallet(String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAddressBookEntry> getAddressBookEntries(Collection<Integer> entryIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int addAddressBookEntry(String address, String description, String paymentId) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void deleteAddressBookEntry(int entryIdx) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void tagAccounts(String tag, Collection<Integer> accountIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void untagAccounts(Collection<Integer> accountIndices) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public List<MoneroAccountTag> getAccountTags() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setAccountTagLabel(String tag, String label) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String createPaymentUri(MoneroSendRequest request) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public MoneroSendRequest parsePaymentUri(String uri) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getOutputsHex() {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public int importOutputsHex(String outputsHex) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void setAttribute(String key, String val) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public String getAttribute(String key) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void startMining(Integer numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
    throw new RuntimeException("Not implemented");
  }

  @Override
  public void stopMining() {
    throw new RuntimeException("Not implemented");
  }
  
  // ------------------------------ NATIVE METHODS ----------------------------
  
  private native static boolean walletExistsJni(String path);
  
  private native static long openWalletJni(String path, String password, int networkType);
  
  private native static long createWalletRandomJni(int networkType, String daemonUrl, String daemonUsername, String daemonPassword, String language);
  
  private native static long createWalletFromMnemonicJni(String mnemonic, int networkType, long restoreHeight);
  
  private native static long createWalletFromKeysJni(String address, String viewKey, String spendKey, int networkType, long restoreHeight, String language);
  
  private native String[] getDaemonConnectionJni(); // returns [uri, username, password]
  
  private native void setDaemonConnectionJni(String uri, String username, String password);
  
  private native String getPathJni();
  
  private native int getNetworkTypeJni();
  
  private native String getLanguageJni();
  
  private native long getHeightJni();
  
  private native long getChainHeightJni();
  
  private native long getRestoreHeightJni();
  
  private native String getMnemonicJni();
  
  private native String getAddressJni(int accountIdx, int subaddressIdx);
  
  private native long setListenerJni(WalletListenerJni listener);
  
  private native void syncJni(long startHeight);
  
  // -------------------------- WALLET LISTENER JNI ---------------------------
  
  /**
   * Interface to receive wallet notifications from C++.
   * 
   * TODO: don't longs lose precision?
   */
  private interface WalletListenerJni {
    
    /**
     * Called when a new block is received.
     * 
     * @param height is the height of the received block
     */
    public void onNewBlock(long height);
    
//    /**
//     * Called when funds are sent from the wallet.
//     * 
//     * @param txId is the id of the outgoing transaction
//     * @param amount is the amount sent from the wallet
//     */
//    public void moneySpent(String txId, long amount);
//    
//    /**
//     * Called when funds are received to the wallet.
//     * 
//     * @param txId is the id of the incoming transaction
//     * @param amount is the amount received to the wallet
//     */
//    public void moneyReceived(String txId, long amount);
//
//    /**
//     * Called when funds are received to the wallet but the tx is still in the tx pool.
//     * 
//     * @param txId is the id of the incoming and unconfirmed transaction
//     * @param amount is the amount received to the wallet
//     */
//    public void unconfirmedMoneyReceived(String txId, long amount);
  }
  
  /**
   * Handles wallet notifications as they are received from C++ over JNI.
   */
  private class WalletListenerJniImpl implements WalletListenerJni {

    @Override
    public void onNewBlock(long height) {
      System.out.println("onNewBlock(" + height + ")");
    }
  }
}