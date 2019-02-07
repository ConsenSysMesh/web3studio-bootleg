module.exports = {
  networks: {
    coverage: {
      host: 'localhost',
      network_id: '*',
      port: 8555,
      // Values set from docs https://github.com/sc-forks/solidity-coverage#network-configuration
      gas: 0xfffffffffff,
      gasPrice: 0x01
    }
  },

  mocha: {
    reporter: 'eth-gas-reporter',
    reporterOptions: {
      currency: 'USD',
      gasPrice: 4
    }
  },

  // Configure your compilers
  compilers: {
    solc: {
      version: '^0.5.3'
    }
  }
};
