const expect = require('jest-matchers');
const BootlegToken = artifacts.require('BootlegToken');

contract('BootlegToken', accounts => {
  const oneEthInWei = web3.utils.toWei('1', 'ether');
  let token;
  let tokenId;

  /**
   * Calculate the token balance of an account
   *
   * @param {string} account - Account to get the balance of
   * @returns {Promise<number>} Returns # of tokens for the account
   */
  async function tokenBalance(account) {
    return (await token.balanceOf(account)).toNumber();
  }

  /**
   * Calculate the franchisor balance of an account
   *
   * @param {string} account - Account to get the balance of
   * @returns {Promise<number>} Returns # of tokens the account is a franchisor of
   */
  async function tokenFranchisorBalance(account) {
    return (await token.franchisorBalanceOf(account)).toNumber();
  }

  /**
   * Calculate the payment balance of an account
   *
   * @param {string} account - Account to get the balance of
   * @returns {Promise<string>} Returns the balance of an account in wei
   */
  async function tokenPaymentBalance(account) {
    return (await token.paymentBalanceOf(account, tokenId)).toString();
  }

  /**
   * Get the number of payments an account has left
   *
   * @param {string} account - Account to get the payments left
   * @returns {Promise<number>} Returns the number of payments left
   */
  async function franchisorWithdrawPaymentsLeft(account) {
    return (await token.franchisorWithdrawPaymentsLeft(
      account,
      tokenId
    )).toNumber();
  }

  /**
   * Get the balance of an account
   *
   * @param {string} account - Account to get the balance
   * @returns {Promise<number>} returns the balance of an account in eth
   */
  async function accountBalance(account) {
    return parseInt(await web3.eth.getBalance(account));
  }

  beforeEach(async () => {
    tokenId = web3.utils.asciiToHex('super awesome token id');
    token = await BootlegToken.new('Bootleg', 'BLEG');

    await token.mintWithTokenURI(
      accounts[0],
      tokenId,
      'https://ipfs.infura.io/ipfs/QmSomeHash'
    );
  });

  it('should support ERC165', async () => {
    expect(await token.supportsInterface('0x01ffc9a7')).toEqual(true);
  });

  it('should be able to transfer a token', async () => {
    expect(await tokenBalance(accounts[0])).toEqual(1);
    expect(await tokenBalance(accounts[1])).toEqual(0);

    await token.transferFrom(accounts[0], accounts[1], tokenId);

    expect(await tokenBalance(accounts[0])).toEqual(0);
    expect(await tokenBalance(accounts[1])).toEqual(1);
  });

  it('should be able to get the correct franchisor balance', async () => {
    expect(await tokenFranchisorBalance(accounts[0])).toEqual(1);
    expect(await tokenFranchisorBalance(accounts[1])).toEqual(0);
    expect(await tokenFranchisorBalance(accounts[2])).toEqual(0);

    await token.transferFrom(accounts[0], accounts[1], tokenId);

    expect(await tokenFranchisorBalance(accounts[0])).toEqual(1);
    expect(await tokenFranchisorBalance(accounts[1])).toEqual(1);
    expect(await tokenFranchisorBalance(accounts[2])).toEqual(0);

    await token.transferFrom(accounts[1], accounts[2], tokenId, {
      from: accounts[1]
    });

    expect(await tokenFranchisorBalance(accounts[0])).toEqual(1);
    expect(await tokenFranchisorBalance(accounts[1])).toEqual(1);
    expect(await tokenFranchisorBalance(accounts[2])).toEqual(1);
  });

  it('should be able to withdraw the correct payment', async () => {
    const startingBalance = await accountBalance(accounts[0]);

    expect(await franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(0);

    await token.transferFrom(accounts[0], accounts[1], tokenId, {
      value: oneEthInWei
    });
    const preWithdrawBalance = await accountBalance(accounts[0]);

    expect(await tokenPaymentBalance(accounts[0])).toEqual(oneEthInWei);

    expect(await franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(1);
    expect(preWithdrawBalance).toBeLessThan(startingBalance);

    await token.withdrawPayment(accounts[0], tokenId);

    expect(await accountBalance(accounts[0])).toBeGreaterThan(
      preWithdrawBalance
    );

    expect(await tokenPaymentBalance(accounts[0])).toEqual('0');
  });

  it("shouldn't be able to withdraw passed max", async () => {
    await token.transferFrom(accounts[0], accounts[1], tokenId, {
      value: oneEthInWei
    });

    const preWithdrawBalance = await accountBalance(accounts[0]);

    const paymentBalance = (await token.paymentBalanceOf(
      accounts[0],
      1,
      400,
      tokenId
    )).toString();

    expect(paymentBalance).toEqual(oneEthInWei);

    expect(await tokenPaymentBalance(accounts[0], 400)).toEqual(oneEthInWei);

    await token.withdrawPayment(accounts[0], 400, tokenId);

    expect(await accountBalance(accounts[0])).toBeGreaterThan(
      preWithdrawBalance
    );

    expect(await tokenPaymentBalance(accounts[0])).toEqual('0');
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
    expect(await franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(0);

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

    await token.transferFrom(accounts[0], accounts[3], tokenId, {
      from: accounts[0],
      value: oneEthInWei
    });

    expect(await franchisorWithdrawPaymentsLeft(accounts[0])).toEqual(4);
    expect(await tokenPaymentBalance(accounts[0])).toEqual(
      web3.utils.toWei('2', 'ether')
    );
  });
});
