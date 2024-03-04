package test;

import monero.wallet.MoneroWallet;
import monero.wallet.MoneroWalletLight;
import monero.wallet.model.MoneroWalletConfig;
import org.junit.jupiter.api.TestInstance;
import utils.TestUtils;

import java.util.List;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)  // so @BeforeAll and @AfterAll can be used on non-static functions

public class TestMoneroWalletLight extends TestMoneroWalletCommon {

    public TestMoneroWalletLight() {
        super();
    }

    @Override
    protected MoneroWallet getTestWallet() {
        return TestUtils.getWalletLight();
    }

    @Override
    protected MoneroWallet openWallet(MoneroWalletConfig config) {
        return MoneroWalletLight.openWallet(config);
    }

    /**
     * Create a test wallet with default configuration for each wallet type.
     *
     * @param config configures the wallet to create
     * @return MoneroWallet is the created wallet
     */
    @Override
    protected MoneroWallet createWallet(MoneroWalletConfig config) {
        return null;
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
        return null;
    }
}
