const expect = require('jest-matchers');
const MetaCoin = artifacts.require('MetaCoin');

contract('MetaCoin', accounts => {
  const accountOne = accounts[0];
  const accountTwo = accounts[1];
  let metaCoinInstance;

  /**
   * Gets account balances for first two accounts
   *
   * @returns {Promise<*[]>} - Promise for account balances
   */
  async function getAccountBalances() {
    return [
      (await metaCoinInstance.getBalance.call(accountOne)).toNumber(),
      (await metaCoinInstance.getBalance.call(accountTwo)).toNumber()
    ];
  }

  beforeEach(async () => {
    metaCoinInstance = await MetaCoin.deployed();
  });

  it('should put 10000 MetaCoin in the first account', async () => {
    const balance = await metaCoinInstance.getBalance.call(accounts[0]);

    assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
  });

  it('should send coin correctly', async () => {
    // Get initial balances of first and second account.
    const [
      accountOneStartingBalance,
      accountTwoStartingBalance
    ] = await getAccountBalances();

    // Make transaction from first account to second.
    const amount = 10;
    await metaCoinInstance.sendCoin(accountTwo, amount, { from: accountOne });

    // Get balances of first and second account after the transactions.
    const [
      accountOneEndingBalance,
      accountTwoEndingBalance
    ] = await getAccountBalances();

    expect(accountOneEndingBalance).toEqual(accountOneStartingBalance - amount);
    expect(accountTwoEndingBalance).toEqual(accountTwoStartingBalance + amount);
  });

  it('should prevent sends without enough balance', async () => {
    // Get initial balances of first and second account.
    const [
      accountOneStartingBalance,
      accountTwoStartingBalance
    ] = await getAccountBalances();

    // Make transaction from first account to second.
    const amount = 1000;
    await metaCoinInstance.sendCoin(accountOne, amount, {
      from: accountTwo
    });

    // Get balances of first and second account after the transactions.
    const [
      accountOneEndingBalance,
      accountTwoEndingBalance
    ] = await getAccountBalances();

    expect(accountOneEndingBalance).toEqual(accountOneStartingBalance);
    expect(accountTwoEndingBalance).toEqual(accountTwoStartingBalance);
  });
});
