package monero.wallet;

import common.utils.GenUtils;
import common.utils.JsonUtils;
import monero.common.MoneroError;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroKeyImage;
import monero.daemon.model.MoneroNetworkType;
import monero.daemon.model.MoneroVersion;
import monero.wallet.model.*;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public class MoneroWalletLight extends MoneroWalletJni {


    public static MoneroWalletLight openWallet(MoneroWalletConfig config) {
        throw new MoneroError("MoneroWalletLight.openWallet not supported yet.");
    }

    /**
     * Indicates if the wallet is view-only, meaning it does not have the private
     * spend key and can therefore only observe incoming outputs.
     *
     * @return {bool} true if the wallet is view-only, false otherwise
     */
    @Override
    public boolean isViewOnly() {
        return true;
    }

    /**
     * Set the wallet's daemon connection
     *
     * @param daemonConnection manages daemon connection information
     */
    @Override
    public void setDaemonConnection(MoneroRpcConnection daemonConnection) {
        throw new MoneroError("MoneroWalletLight.setDaemonConnection(MoneroRpcConnection daemonConnection) is not supported.");
    }

    public void setDaemonConnection(String uri, String port) {
        setDaemonConnection(uri, port, "", "");
    }

    public void setDaemonConnection(String uri, String port, String adminUri, String adminPort) {
        setDaemonConnection(uri, port, adminUri, adminPort, "");
    }

    public void setDaemonConnection(String uri, String port, String adminUri, String adminPort, String token) {
        setDaemonConnectionJni(uri, port, adminUri, adminPort, token);
    }

    /**
     * Get the wallet's daemon connection.
     *
     * @return the wallet's daemon connection
     */
    @Override
    public MoneroRpcConnection getDaemonConnection() {
        throw new MoneroError("MoneroWalletLight.getDaemonConnection() is not supported.");
    }

    /**
     * Set the Tor proxy to the daemon.
     *
     * @param uri the Tor proxy URI
     */
    @Override
    public void setProxyUri(String uri) {
        throw new MoneroError("MoneroWalletLight.setProxyUri(String uri) is not supported.");
    }

    /**
     * Indicates if the wallet is connected a daemon.
     *
     * @return true if the wallet is connected to a daemon, false otherwise
     */
    @Override
    public boolean isConnectedToDaemon() {
        assertNotClosed();
        try {
            return isConnectedToDaemonJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Returns the wallet version.
     *
     * @return the wallet version
     */
    @Override
    public MoneroVersion getVersion() {
        assertNotClosed();
        try {
            String versionJson = getVersionJni();
            return JsonUtils.deserialize(MoneroRpcConnection.MAPPER, versionJson, MoneroVersion.class);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Get the wallet's path.
     *
     * @return the path the wallet can be opened with
     */
    @Override
    public String getPath() {
        throw new MoneroError("MoneroWalletLight.getPath() is not supported");
    }

    /**
     * Get the wallet's mnemonic phrase or seed.
     *
     * @return the wallet's mnemonic phrase or seed.
     */
    @Override
    public String getSeed() {
        throw new MoneroError("MoneroWalletLight.getSeed() is not supported");
    }

    /**
     * Get the language of the wallet's mnemonic phrase or seed.
     *
     * @return the language of the wallet's mnemonic phrase or seed
     */
    @Override
    public String getSeedLanguage() {
        throw new MoneroError("MoneroWalletLight.getSeedLanguages() is not supported");
    }

    /**
     * Get the wallet's private view key.
     *
     * @return the wallet's private view key
     */
    @Override
    public String getPrivateViewKey() {
        assertNotClosed();
        return getPrivateViewKeyJni();
    }

    /**
     * Get the wallet's private spend key.
     *
     * @return the wallet's private spend key
     */
    @Override
    public String getPrivateSpendKey() {
        throw new MoneroError("MoneroWalletLight.getPrivateSpendKey() is not supported");
    }

    /**
     * Get the wallet's public view key.
     *
     * @return the wallet's public view key
     */
    @Override
    public String getPublicViewKey() {
        return null;
    }

    /**
     * Get the wallet's public spend key.
     *
     * @return the wallet's public spend key
     */
    @Override
    public String getPublicSpendKey() {
        return null;
    }

    /**
     * Get the address of a specific subaddress.
     *
     * @param accountIdx    specifies the account index of the address's subaddress
     * @param subaddressIdx specifies the subaddress index within the account
     * @return the receive address of the specified subaddress
     */
    @Override
    public String getAddress(int accountIdx, int subaddressIdx) {
        if (accountIdx != 0 || subaddressIdx != 0) {
            throw new MoneroError("MoneroWalletLight doesn't support accounts and subaddresses");
        }

        return getPrimaryAddressJni();
    }

    /**
     * Get the account and subaddress index of the given address.
     *
     * @param address is the address to get the account and subaddress index from
     * @return the account and subaddress indices
     */
    @Override
    public MoneroSubaddress getAddressIndex(String address) {
        throw new MoneroError("MoneroWalletLight.getAddressIndex(String address) is not supported");
    }

    /**
     * Get an integrated address based on the given standard address and payment
     * ID. Uses the wallet's primary address if an address is not given.
     * Generates a random payment ID if a payment ID is not given.
     *
     * @param standardAddress is the standard address to generate the integrated address from (wallet's primary address if null)
     * @param paymentId       is the payment ID to generate an integrated address from (randomly generated if null)
     * @return the integrated address
     */
    @Override
    public MoneroIntegratedAddress getIntegratedAddress(String standardAddress, String paymentId) {
        throw new MoneroError("MoneroWalletLight.getIntegratedAddress(String standardAddress, String paymentId) is not supported");
    }

    /**
     * Decode an integrated address to get its standard address and payment id.
     *
     * @param integratedAddress is an integrated address to decode
     * @return the decoded integrated address including standard address and payment id
     */
    @Override
    public MoneroIntegratedAddress decodeIntegratedAddress(String integratedAddress) {
        throw new MoneroError("MoneroWalletLight.decodeIntegratedAddress(String integratedAddress) is not supported");
    }

    /**
     * Get the block height that the wallet is synced to.
     *
     * @return the block height that the wallet is synced to
     */
    @Override
    public long getHeight() {
        assertNotClosed();
        return getHeightJni();
    }

    /**
     * Get the blockchain's height.
     *
     * @return the blockchain's height
     */
    @Override
    public long getDaemonHeight() {
        assertNotClosed();
        return getDaemonHeightJni();
    }

    /**
     * Get the blockchain's height by date as a conservative estimate for scanning.
     *
     * @param year  year of the height to get
     * @param month month of the height to get as a number between 1 and 12
     * @param day   day of the height to get as a number between 1 and 31
     * @return the blockchain's approximate height at the given date
     */
    @Override
    public long getHeightByDate(int year, int month, int day) {
        throw new MoneroError("MoneroWalletLight.getHeightByDate(int year, int month, int day) is not supported");
    }

    @Override
    public MoneroSyncResult sync(Long startHeight, MoneroWalletListenerI listener) {
        assertNotClosed();

        if (listener != null) throw new MoneroError("MoneroWalletLight.sync(Long startHeight, MoneroWalletListenerI listener) is not supported");

        if (startHeight == null) startHeight = Math.max(getHeight(), getRestoreHeight());

        // register listener if given
        if (listener != null) addListener(listener);

        // sync wallet and handle exception
        try {
            Object[] results = syncJni(startHeight);
            return new MoneroSyncResult((long) results[0], (boolean) results[1]);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        } finally {
            if (listener != null) removeListener(listener); // unregister listener
        }
    }

    /**
     * Start background synchronizing with a maximum period between syncs.
     *
     * @param syncPeriodInMs maximum period between syncs in milliseconds
     */
    @Override
    public void startSyncing(Long syncPeriodInMs) {
        assertNotClosed();
        try {
            startSyncingJni(syncPeriodInMs == null ? DEFAULT_SYNC_PERIOD_IN_MS : syncPeriodInMs);
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Stop synchronizing the wallet with the daemon.
     */
    @Override
    public void stopSyncing() {
        assertNotClosed();
        try {
            stopSyncingJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Scan transactions by their hash/id.
     *
     * @param txHashes tx hashes to scan
     */
    @Override
    public void scanTxs(Collection<String> txHashes) {
        throw new MoneroError("MoneroWalletLight.scanTxs(Collection<String> txHashes) is not supported");
    }

    /**
     * Rescan the blockchain for spent outputs.
     * <p>
     * Note: this can only be called with a trusted daemon.
     * <p>
     * Example use case: peer multisig hex is import when connected to an untrusted daemon,
     * so the wallet will not rescan spent outputs.  Then the wallet connects to a trusted
     * daemon.  This method should be manually invoked to rescan outputs.
     */
    @Override
    public void rescanSpent() {
        throw new MoneroError("MoneroWalletLight.rescanSpent() is not supported");
    }

    /**
     * Rescan the blockchain from scratch, losing any information which cannot be recovered from
     * the blockchain itself.
     * <p>
     * WARNING: This method discards local wallet data like destination addresses, tx secret keys,
     * tx notes, etc.
     */
    @Override
    public void rescanBlockchain() {
        assertNotClosed();
        try {
            rescanBlockchainJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Get a subaddress's balance.
     *
     * @param accountIdx    index of the account to get the balance of (default all accounts if null)
     * @param subaddressIdx index of the subaddress to get the balance of (default all subaddresses if null)
     * @return the requested balance
     */
    @Override
    public BigInteger getBalance(Integer accountIdx, Integer subaddressIdx) {
        throw new MoneroError("MoneroWalletLight.getBalance(Integer accountIndex, Integer subaddressIdx) is not supported");
    }

    public BigInteger getBalance() {
        return new BigInteger(getBalanceJni());
    }

    /**
     * Get a subaddress's unlocked balance.
     *
     * @param accountIdx    index of the subaddress to get the unlocked balance of (default all accounts if null)
     * @param subaddressIdx index of the subaddress to get the unlocked balance of (default all subaddresses if null)
     * @return the requested unlocked balance
     */
    @Override
    public BigInteger getUnlockedBalance(Integer accountIdx, Integer subaddressIdx) {
        return null;
    }

    public BigInteger getUnlockedBalance() {
        return new BigInteger(getUnlockedBalanceJni());
    }

    /**
     * Get accounts with a given tag.
     *
     * @param includeSubaddresses specifies if subaddresses should be included
     * @param tag                 is the tag for filtering accounts, all accounts if null
     * @return all accounts with the given tag
     */
    @Override
    public List<MoneroAccount> getAccounts(boolean includeSubaddresses, String tag) {
        throw new MoneroError("MoneroWalletLight.getAccounts(boolean includeSubaddresses, String tag) is not supported");
    }

    /**
     * Get an account.
     *
     * @param accountIdx          specifies the account to get
     * @param includeSubaddresses specifies if subaddresses should be included
     * @return the retrieved account
     */
    @Override
    public MoneroAccount getAccount(int accountIdx, boolean includeSubaddresses) {
        throw new MoneroError("MoneroWalletLight.getAccount(int accountIdx, boolean includeSubaddresses is not supported)");
    }

    /**
     * Create a new account with a label for the first subaddress.
     *
     * @param label specifies the label for account's first subaddress (optional)
     * @return the created account
     */
    @Override
    public MoneroAccount createAccount(String label) {
        throw new MoneroError("MoneroWalletLight.createAccount(String label) is not supported");
    }

    /**
     * Get subaddresses in an account.
     *
     * @param accountIdx        specifies the account to get subaddresses within
     * @param subaddressIndices are specific subaddresses to get (optional)
     * @return the retrieved subaddresses
     */
    @Override
    public List<MoneroSubaddress> getSubaddresses(int accountIdx, List<Integer> subaddressIndices) {
        throw new MoneroError("MoneroWalletLight.getSubaddresses(int accountIdx, List<Integer> subaddressIndices) is not supported");
    }

    /**
     * Create a subaddress within an account.
     *
     * @param accountIdx specifies the index of the account to create the subaddress within
     * @param label      specifies the label for the subaddress (optional)
     * @return the created subaddress
     */
    @Override
    public MoneroSubaddress createSubaddress(int accountIdx, String label) {
        throw new MoneroError("MoneroWalletLight.createSubaddresses(int accountIdx, String label) is not supported");
    }

    /**
     * Set a subaddress label.
     *
     * @param accountIdx    index of the account to set the label for
     * @param subaddressIdx index of the subaddress to set the label for
     * @param label         the label to set
     */
    @Override
    public void setSubaddressLabel(int accountIdx, int subaddressIdx, String label) {
        throw new MoneroError("MoneroWalletLight.getSubaddressLabel(int accountIdx, int subaddressIdx, String label) is not supported");
    }

    /**
     * <p>Get wallet transactions that meet the criteria defined in a query object.</p>
     *
     * <p>Transactions must meet every criteria defined in the query in order to
     * be returned.  All criteria are optional and no filtering is applied when
     * not defined.</p>
     *
     * <p>
     * All supported query criteria:<br>
     * &nbsp;&nbsp; isConfirmed - path of the wallet to open<br>
     * &nbsp;&nbsp; password - password of the wallet to open<br>
     * &nbsp;&nbsp; networkType - network type of the wallet to open (one of MoneroNetworkType.MAINNET|TESTNET|STAGENET)<br>
     * &nbsp;&nbsp; serverUri - uri of the wallet's daemon (optional)<br>
     * &nbsp;&nbsp; serverUsername - username to authenticate with the daemon (optional)<br>
     * &nbsp;&nbsp; serverPassword - password to authenticate with the daemon (optional)<br>
     * &nbsp;&nbsp; server - MoneroRpcConnection to a monero daemon (optional)<br>
     * &nbsp;&nbsp; isConfirmed - get txs that are confirmed or not (optional)<br>
     * &nbsp;&nbsp; inTxPool - get txs that are in the tx pool or not (optional)<br>
     * &nbsp;&nbsp; isRelayed - get txs that are relayed or not (optional)<br>
     * &nbsp;&nbsp; isFailed - get txs that are failed or not (optional)<br>
     * &nbsp;&nbsp; isMinerTx - get miner txs or not (optional)<br>
     * &nbsp;&nbsp; hash - get a tx with the hash (optional)<br>
     * &nbsp;&nbsp; hashes - get txs with the hashes (optional)<br>
     * &nbsp;&nbsp; paymentId - get transactions with the payment id (optional)<br>
     * &nbsp;&nbsp; paymentIds - get transactions with the payment ids (optional)<br>
     * &nbsp;&nbsp; hasPaymentId - get transactions with a payment id or not (optional)<br>
     * &nbsp;&nbsp; minHeight - get txs with height greater than or equal to the given height (optional)<br>
     * &nbsp;&nbsp; maxHeight - get txs with height less than or equal to the given height (optional)<br>
     * &nbsp;&nbsp; isOutgoing - get txs with an outgoing transfer or not (optional)<br>
     * &nbsp;&nbsp; isIncoming - get txs with an incoming transfer or not (optional)<br>
     * &nbsp;&nbsp; transferQuery - get txs that have a transfer that meets this query (optional)<br>
     * &nbsp;&nbsp; includeOutputs - specifies that tx outputs should be returned with tx results (optional)<br>
     * </p>
     *
     * @param query specifies properties of the transactions to get
     * @return wallet transactions that meet the query
     */
    @Override
    public List<MoneroTxWallet> getTxs(MoneroTxQuery query) {
        assertNotClosed();

        // copy and normalize tx query up to block
        query = query == null ? new MoneroTxQuery() : query.copy();
        if (query.getBlock() == null) query.setBlock(new MoneroBlock().setTxs(query));

        // serialize query from block and fetch txs from jni
        String blocksJson;
        try {
            //blocksJson = getTxsJni(JsonUtils.serialize(query.getBlock()));
            blocksJson = getTxsJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }

        // deserialize and return txs
        return deserializeTxs(query, blocksJson);
    }

    public List<MoneroTxWallet> getTxs() {
        MoneroTxQuery query = new MoneroTxQuery();

        return getTxs(query);
    }

    /**
     * <p>Get transfers that meet the criteria defined in a query object.</p>
     *
     * <p>Transfers must meet every criteria defined in the query in order to be
     * returned.  All criteria are optional and no filtering is applied when not
     * defined.</p>
     * <p>
     * All supported query criteria:<br>
     * &nbsp;&nbsp; isOutgoing - get transfers that are outgoing or not (optional)<br>
     * &nbsp;&nbsp; isIncoming - get transfers that are incoming or not (optional)<br>
     * &nbsp;&nbsp; address - wallet's address that a transfer either originated from (if outgoing) or is destined for (if incoming) (optional)<br>
     * &nbsp;&nbsp; accountIndex - get transfers that either originated from (if outgoing) or are destined for (if incoming) a specific account index (optional)<br>
     * &nbsp;&nbsp; subaddressIndex - get transfers that either originated from (if outgoing) or are destined for (if incoming) a specific subaddress index (optional)<br>
     * &nbsp;&nbsp; subaddressIndices - get transfers that either originated from (if outgoing) or are destined for (if incoming) specific subaddress indices (optional)<br>
     * &nbsp;&nbsp; amount - amount being transferred (optional)<br>
     * &nbsp;&nbsp; destinations - individual destinations of an outgoing transfer, which is local wallet data and NOT recoverable from the blockchain (optional)<br>
     * &nbsp;&nbsp; hasDestinations - get transfers that have destinations or not (optional)<br>
     * &nbsp;&nbsp; txQuery - get transfers whose transaction meets this query (optional)<br>
     *
     * @param query specifies attributes of transfers to get
     * @return wallet transfers that meet the query
     */
    @Override
    public List<MoneroTransfer> getTransfers(MoneroTransferQuery query) {
        throw new MoneroError("MoneroWalletLight.getTransfers(MoneroTransferQuery query) is not supported");
    }

    /**
     * <p>Get outputs which meet the criteria defined in a query object.</p>
     *
     * <p>Outputs must meet every criteria defined in the query in order to be
     * returned.  All criteria are optional and no filtering is applied when not
     * defined.</p>
     *
     * <p>
     * All supported query criteria:<br>
     * &nbsp;&nbsp; accountIndex - get outputs associated with a specific account index (optional)<br>
     * &nbsp;&nbsp; subaddressIndex - get outputs associated with a specific subaddress index (optional)<br>
     * &nbsp;&nbsp; subaddressIndices - get outputs associated with specific subaddress indices (optional)<br>
     * &nbsp;&nbsp; amount - get outputs with a specific amount (optional)<br>
     * &nbsp;&nbsp; minAmount - get outputs greater than or equal to a minimum amount (optional)<br>
     * &nbsp;&nbsp; maxAmount - get outputs less than or equal to a maximum amount (optional)<br>
     * &nbsp;&nbsp; isSpent - get outputs that are spent or not (optional)<br>
     * &nbsp;&nbsp; keyImage - get outputs that match the fields defined in the given key image (optional)<br>
     * &nbsp;&nbsp; txQuery - get outputs whose transaction meets this filter (optional)<br>
     * </p>
     *
     * @param query specifies attributes of outputs to get
     * @return the queried outputs
     */
    @Override
    public List<MoneroOutputWallet> getOutputs(MoneroOutputQuery query) {
        assertNotClosed();

        // copy and normalize query up to block
        if (query == null) query = new MoneroOutputQuery();
        else {
            if (query.getTxQuery() == null) query = query.copy();
            else {
                MoneroTxQuery txQuery = query.getTxQuery().copy();
                if (query.getTxQuery().getOutputQuery() == query) query = txQuery.getOutputQuery();
                else {
                    GenUtils.assertNull("Output query's tx query must be circular reference or null", query.getTxQuery().getOutputQuery());
                    query = query.copy();
                    query.setTxQuery(txQuery);
                }
            }
        }
        if (query.getTxQuery() == null) query.setTxQuery(new MoneroTxQuery());
        query.getTxQuery().setOutputQuery(query);
        if (query.getTxQuery().getBlock() == null) query.getTxQuery().setBlock(new MoneroBlock().setTxs(query.getTxQuery()));

        // serialize query from block and fetch outputs from jni
        String blocksJson = getOutputsJni(JsonUtils.serialize(query.getTxQuery().getBlock()));
        //String blocksJson = getOutputsJni();
        // deserialize and return outputs
        return deserializeOutputs(query, blocksJson);
    }

    public List<MoneroOutputWallet> getOutputs() {
        MoneroOutputQuery query = new MoneroOutputQuery();

        return getOutputs(query);
    }

    /**
     * Export outputs in hex format.
     *
     * @param all exports all outputs if true, else exports the outputs since the last export
     * @return outputs in hex format
     */
    @Override
    public String exportOutputs(boolean all) {
        throw new MoneroError("MoneroWalletLight.exportOutputs(boolean all) is not supported");
    }

    /**
     * Import outputs in hex format.
     *
     * @param outputsHex are outputs in hex format
     * @return the number of outputs imported
     */
    @Override
    public int importOutputs(String outputsHex) {
        throw new MoneroError("MoneroWalletLight.importOutputs(String outputsHex) is not supported");
    }

    /**
     * Export signed key images.
     *
     * @param all exports all key images if true, else exports the key images since the last export
     * @return signed key images
     */
    @Override
    public List<MoneroKeyImage> exportKeyImages(boolean all) {
        throw new MoneroError("MoneroWalletLight.exportKeyImages(boolean all) is not supported");
    }

    /**
     * Import signed key images and verify their spent status.
     *
     * @param keyImages are key images to import and verify (requires hex and signature)
     * @return results of the import
     */
    @Override
    public MoneroKeyImageImportResult importKeyImages(List<MoneroKeyImage> keyImages) {
        throw new MoneroError("MoneroWalletLight.importKeyImages(List<MoneroKeyImage> keyImages) is not supported");
    }

    /**
     * Get new key images from the last imported outputs.
     *
     * @return the key images from the last imported outputs
     */
    @Override
    public List<MoneroKeyImage> getNewKeyImagesFromLastImport() {
        throw new MoneroError("MoneroWalletLight.getNewKeyImagesFromLastImport() is not supported");
    }

    /**
     * Freeze an output.
     *
     * @param keyImage key image of the output to freeze
     */
    @Override
    public void freezeOutput(String keyImage) {
        throw new MoneroError("MoneroWalletLight.freezeOutput(String keyImage) is not supported");
    }

    /**
     * Thaw a frozen output.
     *
     * @param keyImage key image of the output to thaw
     */
    @Override
    public void thawOutput(String keyImage) {
        throw new MoneroError("MoneroWalletLight.thawOutput(String keyImage) is not supported");
    }

    /**
     * Check if an output is frozen.
     *
     * @param keyImage key image of the output to check if frozen
     * @return true if the output is frozen, false otherwise
     */
    @Override
    public boolean isOutputFrozen(String keyImage) {
        throw new MoneroError("MoneroWalletLight.isOutputFrozen(String keyImage) is not supported");
    }

    /**
     * Create one or more transactions to transfer funds from this wallet.
     *
     * <p>
     * All supported configuration:<br>
     * &nbsp;&nbsp; address - single destination address (required unless `destinations` provided)<br>
     * &nbsp;&nbsp; amount - single destination amount (required unless `destinations` provided)<br>
     * &nbsp;&nbsp; accountIndex - source account index to transfer funds from (required)<br>
     * &nbsp;&nbsp; subaddressIndex - source subaddress index to transfer funds from (optional)<br>
     * &nbsp;&nbsp; subaddressIndices - source subaddress indices to transfer funds from (optional)<br>
     * &nbsp;&nbsp; relay - relay the transactions to peers to commit to the blockchain (default false)<br>
     * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
     * &nbsp;&nbsp; destinations - addresses and amounts in a multi-destination tx (required unless `address` and `amount` provided)<br>
     * &nbsp;&nbsp; paymentId - transaction payment ID (optional)<br>
     * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transactions to unlock (default 0)<br>
     * &nbsp;&nbsp; canSplit - allow funds to be transferred using multiple transactions (default true)<br>
     * </p>
     *
     * @param config configures the transactions to create
     * @return the created transactions
     */
    @Override
    public List<MoneroTxWallet> createTxs(MoneroTxConfig config) {
        throw new MoneroError("MoneroWalletLight.createTxs(MoneroTxConfig config) is not supported");
    }

    /**
     * Sweep an output with a given key image.
     *
     * <p>
     * All supported configuration:<br>
     * &nbsp;&nbsp; address - single destination address (required)<br>
     * &nbsp;&nbsp; keyImage - key image to sweep (required)<br>
     * &nbsp;&nbsp; relay - relay the transaction to peers to commit to the blockchain (default false)<br>
     * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transaction to unlock (default 0)<br>
     * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
     * </p>
     *
     * @param config configures the sweep transaction
     * @return the created transaction
     */
    @Override
    public MoneroTxWallet sweepOutput(MoneroTxConfig config) {
        throw new MoneroError("MoneroWalletLight.sweepOutput(MoneroTxConfig config) is not supported");
    }

    /**
     * Sweep all unlocked funds according to the given config.
     *
     * <p>
     * All supported configuration:<br>
     * &nbsp;&nbsp; address - single destination address (required)<br>
     * &nbsp;&nbsp; accountIndex - source account index to sweep from (optional, defaults to all accounts)<br>
     * &nbsp;&nbsp; subaddressIndex - source subaddress index to sweep from (optional, defaults to all subaddresses)<br>
     * &nbsp;&nbsp; subaddressIndices - source subaddress indices to sweep from (optional)<br>
     * &nbsp;&nbsp; relay - relay the transactions to peers to commit to the blockchain (default false)<br>
     * &nbsp;&nbsp; priority - transaction priority (default MoneroTxPriority.NORMAL)<br>
     * &nbsp;&nbsp; unlockTime - minimum height or timestamp for the transactions to unlock (default 0)<br>
     * &nbsp;&nbsp; sweepEachSubaddress - sweep each subaddress individually if true (default false)<br>
     * </p>
     *
     * @param config is the sweep configuration
     * @return the created transactions
     */
    @Override
    public List<MoneroTxWallet> sweepUnlocked(MoneroTxConfig config) {
        throw new MoneroError("MoneroWalletLight.sweepUnlocked(MoneroTxConfig config) is not supported");
    }

    /**
     * Sweep all unmixable dust outputs back to the wallet to make them easier to spend and mix.
     * <p>
     * NOTE: Dust only exists pre RCT, so this method will throw "no dust to sweep" on new wallets.
     *
     * @param relay specifies if the resulting transaction should be relayed (defaults to false i.e. not relayed)
     * @return the created transactions
     */
    @Override
    public List<MoneroTxWallet> sweepDust(boolean relay) {
        throw new MoneroError("MoneroWalletLight.sweepDust(boolean relay) is not supported");
    }

    /**
     * Relay previously created transactions.
     *
     * @param txMetadatas are transaction metadata previously created without relaying
     * @return the hashes of the relayed txs
     */
    @Override
    public List<String> relayTxs(Collection<String> txMetadatas) {
        assertNotClosed();
        String[] txMetadatasArr = txMetadatas.toArray(new String[txMetadatas.size()]); // convert to array for jni
        try {
            return Arrays.asList(relayTxsJni(txMetadatasArr));
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Describe a tx set containing unsigned or multisig tx hex to a new tx set containing structured transactions.
     *
     * @param txSet is a tx set containing unsigned or multisig tx hex
     * @return the tx set containing structured transactions
     */
    @Override
    public MoneroTxSet describeTxSet(MoneroTxSet txSet) {
        throw new MoneroError("MoneroWalletLight.describeTxSet(MoneroTxSet txSet) is not supported");
    }

    /**
     * Sign unsigned transactions from a view-only wallet.
     *
     * @param unsignedTxHex is unsigned transaction hex from when the transactions were created
     * @return the signed transaction hex
     */
    @Override
    public MoneroTxSet signTxs(String unsignedTxHex) {
        throw new MoneroError("MoneroWalletLight.signTxs(String unsignedTxHex) is not supported");
    }

    /**
     * Submit signed transactions from a view-only wallet.
     *
     * @param signedTxHex is signed transaction hex from signTxs()
     * @return the resulting transaction hashes
     */
    @Override
    public List<String> submitTxs(String signedTxHex) {
        return null;
    }

    /**
     * Sign a message.
     *
     * @param message       the message to sign
     * @param signatureType sign with spend key or view key
     * @param accountIdx    the account index of the message signature (default 0)
     * @param subaddressIdx the subaddress index of the message signature (default 0)
     * @return the signature
     */
    @Override
    public String signMessage(String message, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx) {
        throw new MoneroError("MoneroWalletLight.signMessage(String message, MoneroMessageSignatureType signatureType, int accountIdx, int subaddressIdx) is not supported");
    }

    /**
     * Verify a signature on a message.
     *
     * @param message   is the signed message
     * @param address   is the signing address
     * @param signature is the signature
     * @return the message signature verification result
     */
    @Override
    public MoneroMessageSignatureResult verifyMessage(String message, String address, String signature) {
        throw new MoneroError("MoneroWalletLight.verifyMessage(String message, String address, String signature) is not supported");
    }

    /**
     * Get a transaction's secret key from its hash.
     *
     * @param txHash is the transaction's hash
     * @return is the transaction's secret key
     */
    @Override
    public String getTxKey(String txHash) {
        throw new MoneroError("MoneroWalletLight.getTxKey(String txHash) is not supported");
    }

    /**
     * Check a transaction in the blockchain with its secret key.
     *
     * @param txHash  specifies the transaction to check
     * @param txKey   is the transaction's secret key
     * @param address is the destination public address of the transaction
     * @return the result of the check
     */
    @Override
    public MoneroCheckTx checkTxKey(String txHash, String txKey, String address) {
        throw new MoneroError("MoneroWalletLight.checkTxKey(String txHash, String  txKey, String address) is not supported");
    }

    /**
     * Get a transaction signature to prove it.
     *
     * @param txHash  specifies the transaction to prove
     * @param address is the destination public address of the transaction
     * @param message is a message to include with the signature to further authenticate the proof (optional)
     * @return the transaction signature
     */
    @Override
    public String getTxProof(String txHash, String address, String message) {
        throw new MoneroError("MoneroWalletLight.getTxProof(String txHash, String address, String message) is not supported");
    }

    /**
     * Prove a transaction by checking its signature.
     *
     * @param txHash    specifies the transaction to prove
     * @param address   is the destination public address of the transaction
     * @param message   is a message included with the signature to further authenticate the proof (optional)
     * @param signature is the transaction signature to confirm
     * @return the result of the check
     */
    @Override
    public MoneroCheckTx checkTxProof(String txHash, String address, String message, String signature) {
        throw new MoneroError("MoneroWalletLight.checkTxProof(String txHash, String address, String message, String signature) is not supported");
    }

    /**
     * Generate a signature to prove a spend. Unlike proving a transaction, it does not require the destination public address.
     *
     * @param txHash  specifies the transaction to prove
     * @param message is a message to include with the signature to further authenticate the proof (optional)
     * @return the transaction signature
     */
    @Override
    public String getSpendProof(String txHash, String message) {
        throw new MoneroError("MoneroWalletLight.getSpendProof(String txHash, String message) is not supported");
    }

    /**
     * Prove a spend using a signature. Unlike proving a transaction, it does not require the destination public address.
     *
     * @param txHash    specifies the transaction to prove
     * @param message   is a message included with the signature to further authenticate the proof (optional)
     * @param signature is the transaction signature to confirm
     * @return true if the signature is good, false otherwise
     */
    @Override
    public boolean checkSpendProof(String txHash, String message, String signature) {
        throw new MoneroError("MoneroWalletLight.checkSpendProof(String txHash, String message, String signature) is not supported");
    }

    /**
     * Generate a signature to prove the entire balance of the wallet.
     *
     * @param message is a message included with the signature to further authenticate the proof (optional)
     * @return the reserve proof signature
     */
    @Override
    public String getReserveProofWallet(String message) {
        throw new MoneroError("MoneroWalletLight.getReserveProofWallet(String message) is not supported");
    }

    /**
     * Generate a signature to prove an available amount in an account.
     *
     * @param accountIdx specifies the account to prove ownership of the amount
     * @param amount     is the minimum amount to prove as available in the account
     * @param message    is a message to include with the signature to further authenticate the proof (optional)
     * @return the reserve proof signature
     */
    @Override
    public String getReserveProofAccount(int accountIdx, BigInteger amount, String message) {
        throw new MoneroError("MoneroWalletLight.getReserveProofAccount(int accountIdx, BigInteger amount, String message) is not supported");
    }

    /**
     * Proves a wallet has a disposable reserve using a signature.
     *
     * @param address   is the public wallet address
     * @param message   is a message included with the signature to further authenticate the proof (optional)
     * @param signature is the reserve proof signature to check
     * @return the result of checking the signature proof
     */
    @Override
    public MoneroCheckReserve checkReserveProof(String address, String message, String signature) {
        throw new MoneroError("MoneroWalletLight.checkReserveProof(String address, String message, String signature) is not supported");
    }

    /**
     * Get notes for multiple transactions.
     *
     * @param txHashes identify the transactions to get notes for
     * @return notes for the transactions
     */
    @Override
    public List<String> getTxNotes(List<String> txHashes) {
        throw new MoneroError("MoneroWalletLight.getTxNotes(List<String> txHashes) is not supported");
    }

    /**
     * Set notes for multiple transactions.
     *
     * @param txHashes specify the transactions to set notes for
     * @param notes    are the notes to set for the transactions
     */
    @Override
    public void setTxNotes(List<String> txHashes, List<String> notes) {
        throw new MoneroError("MoneroWalletLight.setTxNotes(List<String> txHashes, List<String> notes) is not supported");
    }

    /**
     * Get address book entries.
     *
     * @param entryIndices are indices of the entries to get (optional)
     * @return the address book entries
     */
    @Override
    public List<MoneroAddressBookEntry> getAddressBookEntries(List<Integer> entryIndices) {
        throw new MoneroError("MoneroWalletLight.getAddressBookEntries(List<Integer> entryIndices) is not supported");
    }

    /**
     * Add an address book entry.
     *
     * @param address     is the entry address
     * @param description is the entry description (optional)
     * @return the index of the added entry
     */
    @Override
    public int addAddressBookEntry(String address, String description) {
        throw new MoneroError("MoneroWalletLight.addAddressBookEntry(String address, String description) is not supported");
    }

    /**
     * Edit an address book entry.
     *
     * @param index          is the index of the address book entry to edit
     * @param setAddress     specifies if the address should be updated
     * @param address        is the updated address
     * @param setDescription specifies if the description should be updated
     * @param description    is the updated description
     */
    @Override
    public void editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description) {
        throw new MoneroError("MoneroWalletLight.editAddressBookEntry(int index, boolean setAddress, String address, boolean setDescription, String description) is not supported");
    }

    /**
     * Delete an address book entry.
     *
     * @param entryIdx is the index of the entry to delete
     */
    @Override
    public void deleteAddressBookEntry(int entryIdx) {
        throw new MoneroError("MoneroWalletLight.deleteAddressBookEntry(int entryIdx) is not supported");
    }

    /**
     * Tag accounts.
     *
     * @param tag            is the tag to apply to the specified accounts
     * @param accountIndices are the indices of the accounts to tag
     */
    @Override
    public void tagAccounts(String tag, Collection<Integer> accountIndices) {
        throw new MoneroError("MoneroWalletLight.tagAccounts(String tag, Collection<Integer> accountIndices) is not supported");
    }

    /**
     * Untag acconts.
     *
     * @param accountIndices are the indices of the accounts to untag
     */
    @Override
    public void untagAccounts(Collection<Integer> accountIndices) {
        throw new MoneroError("MoneroWalletLight.untagAccounts(Collection<Integer> accountIndices) is not supported");
    }

    /**
     * Return all account tags.
     *
     * @return the wallet's account tags
     */
    @Override
    public List<MoneroAccountTag> getAccountTags() {
        throw new MoneroError("MoneroWalletLight.getAccountTags() is not supported");
    }

    /**
     * Sets a human-readable description for a tag.
     *
     * @param tag   is the tag to set a description for
     * @param label is the label to set for the tag
     */
    @Override
    public void setAccountTagLabel(String tag, String label) {
        throw new MoneroError("MoneroWalletLight.setAccountTagLabel(String tag, String label) is not supported");
    }

    /**
     * Creates a payment URI from a send configuration.
     *
     * @param config specifies configuration for a potential tx
     * @return the payment uri
     */
    @Override
    public String getPaymentUri(MoneroTxConfig config) {
        throw new MoneroError("MoneroWalletLight.getPaymentUri(MoneroTxConfig config) is not supported");
    }

    /**
     * Parses a payment URI to a transaction configuration.
     *
     * @param uri is the payment uri to parse
     * @return the send configuration parsed from the uri
     */
    @Override
    public MoneroTxConfig parsePaymentUri(String uri) {
        throw new MoneroError("MoneroWalletLight.parsePaymentUri(String uri) is not supported");
    }

    /**
     * Get an attribute.
     *
     * @param key is the attribute to get the value of
     * @return the attribute's value
     */
    @Override
    public String getAttribute(String key) {
        throw new MoneroError("MoneroWalletLight.getAttribute(String key) is not supported");
    }

    /**
     * Set an arbitrary attribute.
     *
     * @param key is the attribute key
     * @param val is the attribute value
     */
    @Override
    public void setAttribute(String key, String val) {
        throw new MoneroError("MoneroWalletLight.setAttribute(String key, String val) is not supported");
    }

    /**
     * Start mining.
     *
     * @param numThreads       is the number of threads created for mining (optional)
     * @param backgroundMining specifies if mining should occur in the background (optional)
     * @param ignoreBattery    specifies if the battery should be ignored for mining (optional)
     */
    @Override
    public void startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery) {
        throw new MoneroError("MoneroWalletLight.startMining(Long numThreads, Boolean backgroundMining, Boolean ignoreBattery) is not supported");
    }

    /**
     * Stop mining.
     */
    @Override
    public void stopMining() {
        throw new MoneroError("MoneroWalletLight.stopMining() is not supported");
    }

    /**
     * Indicates if importing multisig data is needed for returning a correct balance.
     *
     * @return true if importing multisig data is needed for returning a correct balance, false otherwise
     */
    @Override
    public boolean isMultisigImportNeeded() {
        return false;
    }

    /**
     * Get multisig info about this wallet.
     *
     * @return multisig info about this wallet
     */
    @Override
    public MoneroMultisigInfo getMultisigInfo() {
        throw new MoneroError("MoneroWalletLight.getMultisigInfo() is not supported");
    }

    /**
     * Get multisig info as hex to share with participants to begin creating a
     * multisig wallet.
     *
     * @return this wallet's multisig hex to share with participants
     */
    @Override
    public String prepareMultisig() {
        throw new MoneroError("MoneroWalletLight.prepareMultisig() is not supported");
    }

    /**
     * Make this wallet multisig by importing multisig hex from participants.
     *
     * @param multisigHexes are multisig hex from each participant
     * @param threshold     is the number of signatures needed to sign transfers
     * @param password      is the wallet password
     * @return this wallet's multisig hex to share with participants
     */
    @Override
    public String makeMultisig(List<String> multisigHexes, int threshold, String password) {
        throw new MoneroError("MoneroWalletLight.makeMultisig(List<String> multisigHexes, int threshold, String password) is not supported");
    }

    /**
     * Exchange multisig hex with participants in a M/N multisig wallet.
     * <p>
     * This process must be repeated with participants exactly N-M times.
     *
     * @param multisigHexes are multisig hex from each participant
     * @param password      is the wallet's password // TODO monero-project: redundant? wallet is created with password
     * @return the result which has the multisig's address xor this wallet's multisig hex to share with participants iff not done
     */
    @Override
    public MoneroMultisigInitResult exchangeMultisigKeys(List<String> multisigHexes, String password) {
        throw new MoneroError("MoneroWalletLight.exchangeMultisigKeys(List<String> multisigHexes, String password) is not supported");
    }

    /**
     * Export this wallet's multisig info as hex for other participants.
     *
     * @return this wallet's multisig info as hex for other participants
     */
    @Override
    public String exportMultisigHex() {
        throw new MoneroError("MoneroWalletLight.exportMultisigHex() is not supported");
    }

    /**
     * Import multisig info as hex from other participants.
     *
     * @param multisigHexes are multisig hex from each participant
     * @return the number of outputs signed with the given multisig hex
     */
    @Override
    public int importMultisigHex(List<String> multisigHexes) {
        throw new MoneroError("MoneroWalletLight.importMultisigHex(List<String> multisigHexes) is not supported");
    }

    /**
     * Sign multisig transactions from a multisig wallet.
     *
     * @param multisigTxHex represents unsigned multisig transactions as hex
     * @return the result of signing the multisig transactions
     */
    @Override
    public MoneroMultisigSignResult signMultisigTxHex(String multisigTxHex) {
        throw new MoneroError("MoneroWalletLight.signMultisigTxHex(String multisigTxHex) is not supported");
    }

    /**
     * Submit signed multisig transactions from a multisig wallet.
     *
     * @param signedMultisigTxHex is signed multisig hex returned from signMultisigTxHex()
     * @return the resulting transaction hashes
     */
    @Override
    public List<String> submitMultisigTxHex(String signedMultisigTxHex) {
        throw new MoneroError("MoneroWalletLight.submitMultisigTxHex(String signedMultisigTxHex) is not supported");
    }

    /**
     * Change the wallet password.
     *
     * @param oldPassword is the wallet's old password
     * @param newPassword is the wallet's new password
     */
    @Override
    public void changePassword(String oldPassword, String newPassword) {
        throw new MoneroError("MoneroWalletLight.changePassword(String oldPassword, String newPassword) is not supported");
    }

    @Override
    public void close(boolean save) {
        closeJni(false);
        super.close(save);
    }

    /**
     * Save the wallet at its current path.
     */
    @Override
    public void save() {
        throw new MoneroError("MoneroWalletLight.save() is not supported");
    }

    /**
     * Indicates if this wallet is closed or not.
     *
     * @return true if the wallet is closed, false otherwise
     */
    @Override
    public boolean isClosed() {
        return isClosed;
    }

    /**
     * Indicates if the wallet's daemon is synced with the network.
     *
     * @return true if the daemon is synced with the network, false otherwise
     */
    public boolean isDaemonSynced() {
        assertNotClosed();
        try {
            return isDaemonSyncedJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }

    /**
     * Indicates if the wallet is synced with the daemon.
     *
     * @return true if the wallet is synced with the daemon, false otherwise
     */
    public boolean isSynced() {
        assertNotClosed();
        try {
            return isSyncedJni();
        } catch (Exception e) {
            throw new MoneroError(e.getMessage());
        }
    }


    /**
     * Get the wallet's network type (mainnet, testnet, or stagenet).
     *
     * @return the wallet's network type
     */
    public MoneroNetworkType getNetworkType() {
        assertNotClosed();
        return MoneroNetworkType.values()[getNetworkTypeJni()];
    }

    /**
     * Get the height of the first block that the wallet scans.
     *
     * @return the height of the first block that the wallet scans
     */
    public long getRestoreHeight() {
        assertNotClosed();
        return getRestoreHeightJni();
    }

    /**
     * Set the height of the first block that the wallet scans.
     *
     * @param syncHeight is the height of the first block that the wallet scans
     */
    public void setRestoreHeight(long syncHeight) {
        assertNotClosed();
        setRestoreHeightJni(syncHeight);
    }

    private void assertNotClosed() {
        if (isClosed) throw new MoneroError("Wallet is closed");
    }

    // ------------------------------ NATIVE METHODS ----------------------------

    private native static void setDaemonConnectionJni(String uri, String port, String adminUri, String adminPort, String token);
    private native static boolean isConnectedToDaemonJni();
    private native static boolean isDaemonSyncedJni();
    private native static boolean isSyncedJni();
    private native static String getVersionJni();
    private native static int getNetworkTypeJni();
    private native static String getPrivateViewKeyJni();
    private native static String getPrimaryAddressJni();
    private native static long getHeightJni();
    private native static long getRestoreHeightJni();
    private native static void setRestoreHeightJni(long startHeight);
    private native static long getDaemonHeightJni();
    private native static Object[] syncJni();
    private native static Object[] syncJni(long restoreHeight);
    private native static void startSyncingJni(long syncPeriodInMs);
    private native static void stopSyncingJni();
    private native static void rescanBlockchainJni();
    private native static String getBalanceJni();
    private native static String getUnlockedBalanceJni();
    private native static String getTxsJni();
    private native static String getOutputsJni(String outputQueryJson);
    private native static String relayTxsJni(String[] txMetadatas);
    private native static void closeJni(boolean save);

}
