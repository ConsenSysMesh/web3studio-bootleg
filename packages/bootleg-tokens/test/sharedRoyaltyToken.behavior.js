const utilFactory = require('./erc721UtilFactory');

module.exports = (mintToken, accounts) => {
  const INTERFACE_ID_SRT = '0x68f3a538';
  const INTERFACE_ID_ERC721 = '0x01ffc9a7';

  describe('Behaves like a Shared Royalty Token', () => {
    let token;
    let tokenId;
    let util;

    beforeEach(async () => {
      const result = await mintToken();

      token = result.token;
      tokenId = result.tokenId;

      util = utilFactory(token, tokenId);
    });

    const oneEthInWei = web3.utils.toWei('1', 'ether');

    it('should support interfaces', async () => {
      expect(await token.supportsInterface(INTERFACE_ID_ERC721)).toEqual(true);
      expect(await token.supportsInterface(INTERFACE_ID_SRT)).toEqual(true);
    });

    it('should be able to transfer a token', async () => {
      expect(await util.tokenBalance(accounts[0])).toEqual(1);
      expect(await util.tokenBalance(accounts[1])).toEqual(0);

      await token.transferFrom(accounts[0], accounts[1], tokenId);

      expect(await util.tokenBalance(accounts[0])).toEqual(0);
      expect(await util.tokenBalance(accounts[1])).toEqual(1);
    });

    it('should be able to get the correct franchisor balance', async () => {
      expect(await util.tokenFranchisorBalance(accounts[0])).toEqual(1);
      expect(await util.tokenFranchisorBalance(accounts[1])).toEqual(0);
      expect(await util.tokenFranchisorBalance(accounts[2])).toEqual(0);

      await token.transferFrom(accounts[0], accounts[1], tokenId);

      expect(await util.tokenFranchisorBalance(accounts[0])).toEqual(1);
      expect(await util.tokenFranchisorBalance(accounts[1])).toEqual(1);
      expect(await util.tokenFranchisorBalance(accounts[2])).toEqual(0);

      await token.transferFrom(accounts[1], accounts[2], tokenId, {
        from: accounts[1]
      });

      expect(await util.tokenFranchisorBalance(accounts[0])).toEqual(1);
      expect(await util.tokenFranchisorBalance(accounts[1])).toEqual(1);
      expect(await util.tokenFranchisorBalance(accounts[2])).toEqual(1);
    });

    it('should be able to withdraw the correct payment', async () => {
      const startingBalance = await util.accountBalance(accounts[0]);

      expect(await util.franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(0);

      await token.transferFrom(accounts[0], accounts[1], tokenId, {
        value: oneEthInWei
      });
      const preWithdrawBalance = await util.accountBalance(accounts[0]);

      expect(await util.tokenPaymentBalance(accounts[0])).toEqual(oneEthInWei);

      expect(await util.franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(1);
      expect(preWithdrawBalance).toBeLessThan(startingBalance);

      await token.withdrawPayment(accounts[0], tokenId);

      expect(await util.accountBalance(accounts[0])).toBeGreaterThan(
        preWithdrawBalance
      );

      expect(await util.tokenPaymentBalance(accounts[0])).toEqual('0');
    });

    it("shouldn't be able to withdraw passed max", async () => {
      await token.transferFrom(accounts[0], accounts[1], tokenId, {
        value: oneEthInWei
      });

      const preWithdrawBalance = await util.accountBalance(accounts[0]);

      const paymentBalance = (await token.paymentBalanceOf(
        accounts[0],
        1,
        400,
        tokenId
      )).toString();

      expect(paymentBalance).toEqual(oneEthInWei);

      expect(await util.tokenPaymentBalance(accounts[0], 400)).toEqual(
        oneEthInWei
      );

      await token.withdrawPayment(accounts[0], 400, tokenId);

      expect(await util.accountBalance(accounts[0])).toBeGreaterThan(
        preWithdrawBalance
      );

      expect(await util.tokenPaymentBalance(accounts[0])).toEqual('0');
    });

    it("Not franchisors shouldn't be able to withdraw payment", async () => {
      let error;

      await token.transferFrom(accounts[0], accounts[1], tokenId, {
        value: oneEthInWei
      });

      try {
        await token.withdrawPayment(accounts[5], tokenId);
      } catch (err) {
        error = err;
      }

      expect(error).toBeInstanceOf(Error);
      expect(error.message).toContain(
        'Payments can only be withdrawn by franchisors'
      );
    });

    it('Should allow franchisor cycles', async () => {
      expect(await util.franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(0);

      await token.transferFrom(accounts[0], accounts[1], tokenId, {
        from: accounts[0],
        value: oneEthInWei
      });

      await token.transferFrom(accounts[1], accounts[2], tokenId, {
        from: accounts[1],
        value: oneEthInWei
      });

      // Introduce a cycle
      await token.transferFrom(accounts[2], accounts[0], tokenId, {
        from: accounts[2],
        value: oneEthInWei
      });

      await util.tokenPaymentBalance(accounts[1]);
      await token.transferFrom(accounts[0], accounts[3], tokenId, {
        from: accounts[0],
        value: oneEthInWei
      });

      expect(await util.franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(4);
      expect(await util.tokenPaymentBalance(accounts[0])).toEqual(
        web3.utils.toWei('3.9', 'ether')
      );
      expect(await util.tokenPaymentBalance(accounts[1])).toEqual(
        web3.utils.toWei('.05', 'ether')
      );
      expect(await util.tokenPaymentBalance(accounts[2])).toEqual(
        web3.utils.toWei('.05', 'ether')
      );
      expect(await util.tokenPaymentBalance(accounts[3])).toEqual(
        web3.utils.toWei('0', 'ether')
      );
    });
  });
};
