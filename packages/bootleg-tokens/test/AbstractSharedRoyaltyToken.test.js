const TestSharedRoyaltyToken = artifacts.require('TestSharedRoyaltyToken');
const sharedRoyaltyBehavior = require('./sharedRoyaltyToken.behavior');

contract('AbstractSharedRoyaltyToken', accounts => {
  /**
   * Mints a Bootleg token
   *
   * @returns {Promise<{TestSharedRoyaltyToken}>} - A SharedRoyalty Token instance
   */
  const mintToken = async () => {
    const id = web3.utils.asciiToHex('super awesome token id');
    const instance = await TestSharedRoyaltyToken.new(5);

    await instance.mint(accounts[0], id);

    return { token: instance, tokenId: id };
  };

  sharedRoyaltyBehavior(mintToken, accounts);
});
