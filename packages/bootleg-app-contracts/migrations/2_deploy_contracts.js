const BootlegToken = artifacts.require('BootlegToken');

module.exports = function(deployer) {
  deployer.deploy(BootlegToken);
};
