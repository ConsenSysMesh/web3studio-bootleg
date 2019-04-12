const BootlegTraderApp = artifacts.require('BootlegTraderApp');
const BootlegToken = artifacts.require('BootlegToken');

contract('BootlegTraderApp', accounts => {
  let tokenContract;
  let app;
  let result;
  const tokenId = web3.utils.asciiToHex('super awesome token id');
  const mockURI = 'https://ipfs.infura.io/ipfs/QmSomeHash';
  const firstOwner = accounts[0];
  const secondOwner = accounts[1];
  const thirdOwner = accounts[2];
  const notAnOwner = accounts[9];
  const oneEthInWei = web3.utils.toWei('1', 'ether');
  const twoEthInWei = web3.utils.toWei('2', 'ether');

  beforeEach(async () => {
    tokenContract = await BootlegToken.new('Bootleg', 'BLEG', 5);
    app = await BootlegTraderApp.new(tokenContract.address);
    await tokenContract.mintWithTokenURI(firstOwner, tokenId, mockURI);
    await tokenContract.approve(app.address, tokenId);
    await tokenContract.setAp;
  });

  describe('with a zero price', async () => {
    it('has a price of zero by default', async () => {
      expect((await app.getTokenPrice(tokenId)).toString()).toEqual('0');
    });

    it('cannot be purchased with a zero price', async () => {
      let errorMsg = '';

      try {
        await app.purchase(tokenId, secondOwner, {
          value: oneEthInWei
        });
      } catch (e) {
        errorMsg = e.toString();
      }

      expect(errorMsg).toMatch(/must have non-zero price/i);
    });
  });

  describe('purchasing a token for the asking price', async () => {
    beforeEach(async () => {
      await app.setTokenPrice(tokenId, oneEthInWei);
      result = await app.purchase(tokenId, secondOwner, {
        value: oneEthInWei
      });
    });

    it('is owned by the second owner', async () => {
      expect(await tokenContract.ownerOf(tokenId)).toEqual(secondOwner);
    });

    it('the payment is registered with the token', async () => {
      const balance = await tokenContract.paymentBalanceOf(firstOwner, tokenId);
      expect(parseInt(balance)).toBeGreaterThan(0);
    });

    it('emits a Purchase event', async () => {
      expect(result.logs[0]['event']).toEqual('Purchased');
    });

    describe('purchasing again by someone else', async () => {
      beforeEach(async () => {
        result = await app.purchase(tokenId, thirdOwner, {
          value: oneEthInWei
        });
      });

      it('is owned by the third owner', async () => {
        expect(await tokenContract.ownerOf(tokenId)).toEqual(secondOwner);
      });
    });
  });

  it('does not allow purchase for no eth', async () => {
    let errorMsg = '';

    try {
      await app.setTokenPrice(tokenId, oneEthInWei);
      result = await app.purchase(tokenId, secondOwner, {
        value: 0
      });
    } catch (e) {
      errorMsg = e.toString();
    }

    expect(errorMsg).toMatch(/must provide eth/i);
  });

  it('allows the owner to change the price', async () => {
    await app.setTokenPrice(tokenId, twoEthInWei);
    expect((await app.getTokenPrice(tokenId)).toString()).toEqual(twoEthInWei);
  });

  it('does not allow non-owner to change the price', async () => {
    let errorMsg = '';

    try {
      await app.setTokenPrice(tokenId, twoEthInWei, { from: notAnOwner });
    } catch (e) {
      errorMsg = e.toString();
    }

    expect(errorMsg).toMatch(/only the owner/i);
  });

  describe('after changing the price of the token', async () => {
    beforeEach(async () => {
      result = await app.setTokenPrice(tokenId, oneEthInWei);
    });

    it('emits a PriceChange event', async () => {
      expect(result.logs[0]['event']).toEqual('PriceChanged');
    });
  });
});
