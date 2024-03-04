package monero.wallet;

import com.fasterxml.jackson.annotation.JsonProperty;
import common.utils.JsonUtils;
import monero.common.MoneroRpcConnection;
import monero.common.MoneroUtils;
import monero.daemon.model.MoneroBlock;
import monero.daemon.model.MoneroTx;
import monero.wallet.model.MoneroOutputQuery;
import monero.wallet.model.MoneroOutputWallet;
import monero.wallet.model.MoneroTxQuery;
import monero.wallet.model.MoneroTxWallet;

import java.math.BigInteger;
import java.util.*;
import java.util.logging.Logger;

public abstract class MoneroWalletJni extends MoneroWalletDefault {
    // ----------------------------- PRIVATE SETUP ------------------------------

    // load monero-project C++ as a dynamic library
    static {
        MoneroUtils.loadNativeLibrary();
    }

    // class variables
    protected static final Logger LOGGER = Logger.getLogger(MoneroWalletJni.class.getName());

    // instance variables
    protected long jniWalletHandle;                 // memory address of the wallet in c++; this variable is read directly by name in c++
    protected long jniListenerHandle;               // memory address of the wallet listener in c++; this variable is read directly by name in c++
    protected WalletJniListener jniListener;        // receives notifications from jni c++
    protected String password;

    /**
     * Private constructor with a handle to the memory address of the wallet in c++.
     *
     * @param jniWalletHandle memory address of the wallet in c++
     */
    protected MoneroWalletJni(long jniWalletHandle, String password) {
        this.jniWalletHandle = jniWalletHandle;
        this.jniListener = new WalletJniListener();
    }

    public MoneroWalletJni() {
        super();
    }

    protected static final long DEFAULT_SYNC_PERIOD_IN_MS = 10000; // default period betweeen syncs in ms

    /**
     * Override MoneroBlock with wallet types for polymorphic deserialization.
     */
    protected static class MoneroBlockWallet extends MoneroBlock {

        // default constructor necessary for serialization
        @SuppressWarnings("unused")
        public MoneroBlockWallet() {
            super();
        }

        @JsonProperty("txs")
        public MoneroBlockWallet setTxWallets(List<MoneroTxWallet> txs) {
            super.setTxs(new ArrayList<MoneroTx>(txs));
            return this;
        }

        /**
         * Initializes a new MoneroBlock with direct references to this block.
         *
         * TODO: more efficient way to deserialize directly into MoneroBlock?
         *
         * @return MoneroBlock is the newly initialized block with direct references to this block
         */
        public MoneroBlock toBlock() {
            MoneroBlock block = new MoneroBlock();
            block.setHash(getHash());
            block.setHeight(getHeight());
            block.setTimestamp(getTimestamp());
            block.setSize(getSize());
            block.setWeight(getWeight());
            block.setLongTermWeight(getLongTermWeight());
            block.setDepth(getDepth());
            block.setDifficulty(getDifficulty());
            block.setCumulativeDifficulty(getCumulativeDifficulty());
            block.setMajorVersion(getMajorVersion());
            block.setMinorVersion(getMinorVersion());
            block.setNonce(getNonce());
            block.setMinerTxHash(getMinerTxHash());
            block.setNumTxs(getNumTxs());
            block.setOrphanStatus(getOrphanStatus());
            block.setPrevHash(getPrevHash());
            block.setReward(getReward());
            block.setPowHash(getPowHash());
            block.setHex(getHex());
            block.setMinerTx(getMinerTx());
            block.setTxs(getTxs());
            block.setTxHashes(getTxHashes());
            for (MoneroTx tx : getTxs()) tx.setBlock(block);  // re-assign tx block references
            return block;
        }
    }

    protected static class BlocksWalletContainer {
        public List<MoneroBlockWallet> blocks;
    }

    protected static class DeserializedBlocksContainer {
        public List<MoneroBlock> blocks;
    }

    protected static DeserializedBlocksContainer deserializeBlocks(String blocksJson) {
        DeserializedBlocksContainer deserializedBlocksContainer = new DeserializedBlocksContainer();
        deserializedBlocksContainer.blocks = new ArrayList<>();
        BlocksWalletContainer blocksWalletContainer = JsonUtils.deserialize(MoneroRpcConnection.MAPPER, blocksJson, MoneroWalletFull.BlocksWalletContainer.class);
        if (blocksWalletContainer.blocks != null) for (MoneroBlockWallet blockWallet : blocksWalletContainer.blocks) deserializedBlocksContainer.blocks.add(blockWallet.toBlock());
        return deserializedBlocksContainer;
    }

    protected static List<MoneroTxWallet> deserializeTxs(MoneroTxQuery query, String blocksJson) {

        // deserialize blocks
        DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
        List<MoneroBlock> blocks = deserializedBlocks.blocks;

        // collect txs
        List<MoneroTxWallet> txs = new ArrayList<MoneroTxWallet>();
        for (MoneroBlock block : blocks) {
            sanitizeBlock(block);
            for (MoneroTx tx : block.getTxs()) {
                if (block.getHeight() == null) tx.setBlock(null); // dereference placeholder block for unconfirmed txs
                txs.add((MoneroTxWallet) tx);
            }
        }

        // re-sort txs which is lost over jni serialization
        if (query.getHashes() != null) {
            Map<String, MoneroTxWallet> txMap = new HashMap<String, MoneroTxWallet>();
            for (MoneroTxWallet tx : txs) txMap.put(tx.getHash(), tx);
            List<MoneroTxWallet> txsSorted = new ArrayList<MoneroTxWallet>();
            for (String txHash : query.getHashes()) if (txMap.containsKey(txHash)) txsSorted.add(txMap.get(txHash));
            txs = txsSorted;
        }
        return txs;
    }

    protected static List<MoneroOutputWallet> deserializeOutputs(MoneroOutputQuery query, String blocksJson) {

        // deserialize blocks
        DeserializedBlocksContainer deserializedBlocks = deserializeBlocks(blocksJson);
        List<MoneroBlock> blocks = deserializedBlocks.blocks;

        // collect outputs
        List<MoneroOutputWallet> outputs = new ArrayList<MoneroOutputWallet>();
        for (MoneroBlock block : blocks) {
            sanitizeBlock(block);
            for (MoneroTx tx : block.getTxs()) {
                outputs.addAll(((MoneroTxWallet) tx).getOutputsWallet());
            }
        }
        return outputs;
    }

    protected static MoneroBlock sanitizeBlock(MoneroBlock block) {
        for (MoneroTx tx : block.getTxs()) sanitizeTxWallet((MoneroTxWallet) tx);
        return block;
    }

    protected static MoneroTxWallet sanitizeTxWallet(MoneroTxWallet tx) {
        return tx;
    }



    // -------------------------------- LISTENER --------------------------------

    /**
     * Receives notifications directly from jni c++.
     */
    @SuppressWarnings("unused") // called directly from jni c++
    protected class WalletJniListener {

        public void onSyncProgress(long height, long startHeight, long endHeight, double percentDone, String message) {
            announceSyncProgress(height, startHeight, endHeight, percentDone, message);
        }

        public void onNewBlock(long height) {
            announceNewBlock(height);
        }

        public void onBalancesChanged(String newBalanceStr, String newUnlockedBalanceStr) {
            announceBalancesChanged(new BigInteger(newBalanceStr), new BigInteger(newUnlockedBalanceStr));
        }

        public void onOutputReceived(long height, String txHash, String amountStr, int accountIdx, int subaddressIdx, int version, String unlockTimeStr, boolean isLocked) {

            // build output to announce
            MoneroOutputWallet output = new MoneroOutputWallet();
            output.setAmount(new BigInteger(amountStr));
            output.setAccountIndex(accountIdx);
            output.setSubaddressIndex(subaddressIdx);
            MoneroTxWallet tx = new MoneroTxWallet();
            tx.setHash(txHash);
            tx.setVersion(version);
            tx.setUnlockTime(new BigInteger(unlockTimeStr));
            output.setTx(tx);
            tx.setOutputs(Arrays.asList(output));
            tx.setIsIncoming(true);
            tx.setIsLocked(isLocked);
            if (height > 0) {
                MoneroBlock block = new MoneroBlock().setHeight(height);
                block.setTxs(Arrays.asList(tx));
                tx.setBlock(block);
                tx.setIsConfirmed(true);
                tx.setInTxPool(false);
                tx.setIsFailed(false);
            } else {
                tx.setIsConfirmed(false);
                tx.setInTxPool(true);
            }

            // announce output
            announceOutputReceived((MoneroOutputWallet) tx.getOutputs().get(0));
        }

        public void onOutputSpent(long height, String txHash, String amountStr, String accountIdxStr, String subaddressIdxStr, int version, String unlockTimeStr, boolean isLocked) {

            // build spent output
            MoneroOutputWallet output = new MoneroOutputWallet();
            output.setAmount(new BigInteger(amountStr));
            if (accountIdxStr.length() > 0) output.setAccountIndex(Integer.parseInt(accountIdxStr));
            if (subaddressIdxStr.length() > 0) output.setSubaddressIndex(Integer.parseInt(subaddressIdxStr));
            MoneroTxWallet tx = new MoneroTxWallet();
            tx.setHash(txHash);
            tx.setVersion(version);
            tx.setUnlockTime(new BigInteger(unlockTimeStr));
            tx.setIsLocked(isLocked);
            output.setTx(tx);
            tx.setInputs(Arrays.asList(output));
            tx.setIsIncoming(false);
            if (height > 0) {
                MoneroBlock block = new MoneroBlock().setHeight(height);
                block.setTxs(Arrays.asList(tx));
                tx.setBlock(block);
                tx.setIsConfirmed(true);
                tx.setInTxPool(false);
                tx.setIsFailed(false);
            } else {
                tx.setIsConfirmed(false);
                tx.setInTxPool(true);
            }

            // announce output
            announceOutputSpent((MoneroOutputWallet) tx.getInputs().get(0));
        }
    }

}
