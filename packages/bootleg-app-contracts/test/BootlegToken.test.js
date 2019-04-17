const BootlegToken = artifacts.require('BootlegToken');
const sharedRoyaltyBehavior = require('bootleg-tokens/test/sharedRoyaltyToken.behavior');
const utilFactory = require('bootleg-tokens/test/erc721UtilFactory');

contract('BootlegToken', accounts => {
  let token;
  let tokenId;
  let util;

  const INTERFACE_ID_ERC721_METADATA = '0x5b5e139f';
  const INTERFACE_ID_ERC721_ENUMERABLE = '0x780e9d63';

  /**
   * Mints a Bootleg token
   *
   * @returns {Promise<{BootlegToken}>} - A Bootleg Token instance
   */
  const mintToken = async () => {
    const id = web3.utils.asciiToHex('super awesome token id');
    const instance = await BootlegToken.new('Bootleg', 'BLEG', 5);

    await instance.mintWithTokenURI(
      accounts[0],
      id,
      'https://ipfs.infura.io/ipfs/QmSomeHash'
    );

    return { token: instance, tokenId: id };
  };

  sharedRoyaltyBehavior(mintToken, accounts);

  beforeEach(async () => {
    const result = await mintToken();

    token = result.token;
    tokenId = result.tokenId;
    util = utilFactory(token, tokenId);
  });

  it('should support extended interfaces', async () => {
    expect(await token.supportsInterface(INTERFACE_ID_ERC721_METADATA)).toEqual(
      true
    );

    expect(
      await token.supportsInterface(INTERFACE_ID_ERC721_ENUMERABLE)
    ).toEqual(true);
  });

  it('should allow the minter to add more franchisors', async () => {
    await token.addFranchisor(accounts[1], tokenId, {
      value: web3.utils.toWei('1', 'ether')
    });

    await token.addFranchisor(accounts[2], tokenId, {
      value: web3.utils.toWei('1', 'ether')
    });

    expect(await util.franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(2);
    expect(await util.franchisorWithdrawPaymentsLeft(accounts[1])).toEqual(1);
    expect(await util.franchisorWithdrawPaymentsLeft(accounts[2])).toEqual(0);
  });

  it('non minters can not add new franchisors without transfer', async () => {
    let error;

    try {
      await token.addFranchisor(accounts[1], tokenId, {
        value: web3.utils.toWei('1', 'ether'),
        from: accounts[2]
      });
    } catch (e) {
      error = e;
    }

    expect(error).toBeInstanceOf(Error);
  });

  it('allows setting of a trader App to trade the token', async () => {
    await token.setTraderApp(accounts[8]);
    expect(await token.traderApp()).toEqual(accounts[8]);
  });

  it('only allows the minter to set the Trading app', async () => {
    let errorMsg = '';

    try {
      const someRando = await web3.eth.accounts.create();
      await token.setTraderApp(someRando.address, { from: someRando.address });
    } catch (e) {
      errorMsg = e.toString();
    }

    expect(errorMsg).toMatch(/account not recognized/i);
  });
});
