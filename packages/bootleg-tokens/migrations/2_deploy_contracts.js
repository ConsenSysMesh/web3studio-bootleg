const TestSharedRoyaltyToken = artifacts.require('TestSharedRoyaltyToken');

module.exports = function(deployer) {
  deployer.deploy(TestSharedRoyaltyToken);
};
