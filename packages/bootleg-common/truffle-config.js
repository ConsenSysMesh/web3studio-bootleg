const ProviderEngine = require('web3-provider-engine');
const WebsocketProvider = require('web3-provider-engine/subproviders/websocket.js');
const {
  TruffleArtifactAdapter,
  CoverageSubprovider
} = require('@0x/sol-coverage');

const solcVersion = '0.5.4';
const defaultFromAddress = '0x627306090abab3a6e1400e9345bc60c78a8bef57';

module.exports = projectRoot => {
  const provider = new ProviderEngine();
  const artifactAdapter = new TruffleArtifactAdapter(
    `${projectRoot}`,
    solcVersion
  );
  const coverageSubProvider = new CoverageSubprovider(
    artifactAdapter,
    defaultFromAddress,
    true
  );

  let providerStarted = false;

  global.coverageSubProvider = coverageSubProvider;

  provider.addProvider(coverageSubProvider);

  provider.send = provider.sendAsync.bind(provider);

  return {
    networks: {
      test: {
        provider: () => {
          if (!providerStarted) {
            // Within this function to not start the provider until it's needed
            provider.addProvider(
              new WebsocketProvider({ rpcUrl: 'http://localhost:8545' })
            );

            provider.start(err => {
              providerStarted = true;
              if (err !== undefined) {
                // eslint-disable-next-line no-console
                console.log('Failed to start provider', err);
                process.exit(1);
              }
            });
          }

          return provider;
        },
        network_id: '*'
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
