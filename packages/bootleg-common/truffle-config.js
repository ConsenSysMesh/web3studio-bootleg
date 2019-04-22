const ProviderEngine = require('web3-provider-engine');
const WebsocketProvider = require('web3-provider-engine/subproviders/websocket.js');
const {
  TruffleArtifactAdapter,
  CoverageSubprovider
} = require('@0x/sol-coverage');
const { RevertTraceSubprovider } = require('@0x/sol-trace');
const HDWalletProvider = require('truffle-hdwallet-provider');
const Web3 = require('web3');

require('dotenv').config();

const mnemonic = process.env.SEED_PHRASE;
const infuraApiKey = process.env.INFURA_API_KEY;

const solcVersion = '0.5.4';
const defaultFromAddress = '0x627306090abab3a6e1400e9345bc60c78a8bef57';
const isVerbose = true;

/**
 * Create's a test provider using 0x tracing and coverage
 *
 * @param {string} projectRoot - path of project using this config
 * @returns {ProviderEngine} A web3 provider
 * @private
 */
const createTestProvider = projectRoot => {
  const testProvider = new ProviderEngine();
  const artifactAdapter = new TruffleArtifactAdapter(
    `${projectRoot}`,
    solcVersion
  );
  const coverageSubProvider = new CoverageSubprovider(
    artifactAdapter,
    defaultFromAddress,
    isVerbose
  );

  global.coverageSubProvider = coverageSubProvider;

  testProvider.addProvider(coverageSubProvider);

  testProvider.addProvider(
    new RevertTraceSubprovider(artifactAdapter, defaultFromAddress, isVerbose)
  );

  testProvider.send = testProvider.sendAsync.bind(testProvider);

  return testProvider;
};

/**
 * Create's a provider for a given network
 *
 * @param {string} network - Infura network string (rinkeby, ropsten, mainnet, etc)
 * @returns {Function} function used for truffle config
 * @private
 */
const infuraProvider = network => () =>
  new HDWalletProvider(
    mnemonic,
    `https://${network}.infura.io/v3/${infuraApiKey}`
  );

module.exports = projectRoot => {
  const testProvider = createTestProvider(projectRoot);
  let testProviderStarted = false;

  return {
    networks: {
      development: {
        host: '127.0.0.1',
        port: 7545,
        network_id: '*' // Match any network id
      },
      test: {
        provider: () => {
          if (!testProviderStarted) {
            // Within this function to not start the provider until it's needed
            testProvider.addProvider(
              new WebsocketProvider({ rpcUrl: 'http://localhost:8545' })
            );

            testProvider.start(err => {
              testProviderStarted = true;

              if (err !== undefined) {
                // eslint-disable-next-line no-console
                console.log('Failed to start provider', err);
                process.exit(1);
              }
            });
          }

          return testProvider;
        },
        network_id: '*'
      },
      mainnet: {
        provider: infuraProvider('mainnet'),
        network_id: '1',
        // When deploying to mainnet check https://ethgasstation.info/ for
        // current gas price. "Standard" suggested
        gasPrice: Web3.utils.toWei('0', 'gwei')
      },
      ropsten: {
        provider: infuraProvider('ropsten'),
        network_id: '3',
        gasPrice: Web3.utils.toWei('20', 'gwei')
      },
      rinkeby: {
        provider: infuraProvider('rinkeby'),
        network_id: '4',
        gasPrice: Web3.utils.toWei('10', 'gwei')
      }
    },
    // Configure your compilers
    compilers: {
      solc: {
        version: solcVersion
      }
    }
  };
};
