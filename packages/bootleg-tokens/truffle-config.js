module.exports = require('web3studio-helpers/truffle-config')(__dirname, {
  solcVersion: '0.5.4',
  ignoreFilesGlobs: [
    '**/node_modules/**',
    '**/Migrations.sol',
    '**/contracts/ERC721/**.sol',
    '**/contracts/I*.sol'
  ]
});
