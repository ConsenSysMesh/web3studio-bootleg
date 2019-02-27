module.exports = (token, tokenId) => ({
  /**
   * Calculate the token balance of an account
   *
   * @param {string} account - Account to get the balance of
   * @returns {Promise<number>} Returns # of tokens for the account
   */
  tokenBalance: async account => {
    return (await token.balanceOf(account)).toNumber();
  },

  /**
   * Calculate the franchisor balance of an account
   *
   * @param {string} account - Account to get the balance of
   * @returns {Promise<number>} Returns # of tokens the account is a franchisor of
   */
  tokenFranchisorBalance: async account => {
    return (await token.franchisorBalanceOf(account)).toNumber();
  },

  /**
   * Calculate the payment balance of an account
   *
   * @param {string} account - Account to get the balance of
   * @returns {Promise<string>} Returns the balance of an account in wei
   */
  tokenPaymentBalance: async account => {
    return (await token.paymentBalanceOf(account, tokenId)).toString();
  },

  /**
   * Get the number of payments an account has left
   *
   * @param {string} account - Account to get the payments left
   * @returns {Promise<number>} Returns the number of payments left
   */
  franchisorWithdrawPaymentsLeft: async account => {
    return (await token.franchisorWithdrawPaymentsLeft(
      account,
      tokenId
    )).toNumber();
  },

  /**
   * Get the balance of an account
   *
   * @param {string} account - Account to get the balance
   * @returns {Promise<number>} returns the balance of an account in eth
   */
  accountBalance: async account => {
    return parseInt(await web3.eth.getBalance(account));
  }
});
