module.exports = require('web3studio-helpers/truffle-config')(__dirname, {
  ignoreFilesGlobs: [
    '**/node_modules/**',
    '**/Migrations.sol',
    '**/contracts/ERC721/**.sol',
    '**/contracts/I*.sol'
  ]
});
