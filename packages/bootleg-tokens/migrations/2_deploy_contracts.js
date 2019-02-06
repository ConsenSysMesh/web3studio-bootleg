const MetaCoin = artifacts.require('MetaCoin');

module.exports = function(deployer) {
  deployer.deploy(MetaCoin);
};
