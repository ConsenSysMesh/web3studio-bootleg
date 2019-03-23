const expect = require('jest-matchers');
const sharedRoyaltyBehavior = require('./sharedRoyaltyToken.behavior');
const BookendSharedRoyaltyToken = artifacts.require('TestSharedRoyaltyToken');

contract('BookendSharedRoyaltyToken', accounts => {
  const accountOne = accounts[0];
  const accountTwo = accounts[1];
  const accountThree = accounts[2];
  const tokenId = 1;

  let token;
  let balance;

  /**
   * Mints a Bootleg token
   *
   * @returns {Promise<{TestSharedRoyaltyToken}>} - A SharedRoyalty Token instance
   */
  const mintToken = async () => {
    token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);

    return { token, tokenId };
  };

  sharedRoyaltyBehavior(mintToken, accounts);

  beforeEach(async () => {
    await mintToken();
  });

  describe('the first transfer', async () => {
    it('pays the accountOne full payment on first transfer', async () => {
      await token.transferFrom(accountOne, accountTwo, tokenId, {
        value: 10
      });
      balance = await token.paymentBalanceOf(accountOne, 1, 1, tokenId);
      expect(parseInt(balance)).toEqual(10);
    });

    describe('rounding error correction', async () => {
      it('rounds up the payment amount for accountOne', async () => {
        await token.transferFrom(accountOne, accountTwo, tokenId, {
          value: 7
        });
        balance = await token.paymentBalanceOf(accountOne, 1, 1, tokenId);
        expect(parseInt(balance)).toEqual(7);
      });
    });
  });

  describe('the second transfer', async () => {
    beforeEach(async () => {
      await token.transferFrom(accountOne, accountTwo, tokenId, {
        value: 10
      });
      await token.withdrawPayment(accountOne, 1, tokenId);
    });

    it('pays accountOne a fraction of the second transfer payment', async () => {
      await token.transferFrom(accountTwo, accountThree, tokenId, {
        from: accountTwo,
        value: 10
      });
      balance = await token.paymentBalanceOf(accountOne, 2, 1, tokenId);
      expect(parseInt(balance)).toEqual(9);
    });

    describe('with a larger payment amount', async () => {
      beforeEach(async () => {
        await token.transferFrom(accountTwo, accountThree, tokenId, {
          from: accountTwo,
          value: 100
        });
      });

      it('pays accountOne a fraction of the second transfer with larger payment', async () => {
        balance = await token.paymentBalanceOf(accountOne, 2, 1, tokenId);
        expect(parseInt(balance)).toEqual(95);
      });

      it('pays accountTwo the franchisorPercentage amount on the second transfer', async () => {
        balance = await token.paymentBalanceOf(accountTwo, 2, 1, tokenId);
        expect(parseInt(balance)).toEqual(5);
      });

      it('does not pay accountThree on second transfer', async () => {
        balance = await token.paymentBalanceOf(accountThree, 3, 1, tokenId);
        expect(parseInt(balance)).toEqual(0);
      });
    });

    describe('rounding error correction', async () => {
      it('rounds up payment for accountTwo', async () => {
        await token.transferFrom(accountTwo, accountThree, tokenId, {
          from: accountTwo,
          value: 101
        });
        balance = await token.paymentBalanceOf(accountTwo, 2, 1, tokenId);
        expect(parseInt(balance)).toEqual(6);
      });

      it('rounds up payments for accountThree', async () => {
        await token.transferFrom(accountTwo, accountThree, tokenId, {
          from: accountTwo,
          value: 101
        });
        balance = await token.paymentBalanceOf(accountThree, 3, 1, tokenId);
        expect(parseInt(balance)).toEqual(0);
      });
    });
  });

  describe('PaymentBalanceOf - requesting count larger than payment length', async () => {
    it('returns expected total balance for accountOne', async () => {
      await token.transferFrom(accountOne, accountTwo, tokenId, {
        value: 10
      });
      balance = await token.paymentBalanceOf(accountOne, 1, 50, tokenId);
      expect(parseInt(balance)).toEqual(10);
    });

    it('returns expected total balance for accountTwo', async () => {
      await token.transferFrom(accountOne, accountTwo, tokenId, {
        value: 10
      });
      await token.transferFrom(accountTwo, accountThree, tokenId, {
        from: accountTwo,
        value: 101
      });
      balance = await token.paymentBalanceOf(accountTwo, 2, 5, tokenId);
      expect(parseInt(balance)).toEqual(6);
    });
  });
});
