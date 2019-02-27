const expect = require('jest-matchers');
const BookendSharedRoyaltyToken = artifacts.require(
  'BookendSharedRoyaltyToken'
);

contract('BookendSharedRoyaltyToken', accounts => {
  const accountOne = accounts[0];
  const accountTwo = accounts[1];
  const accountThree = accounts[2];
  const tokenId = 1;
  beforeEach(async () => {
    token = await BookendSharedRoyaltyToken.new();
    mint = await token.mint(accountOne, tokenId);
  });

  it('should return amount of first sale for minter', async () => {
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 5
    });
    event = await token.withdrawPayment(accountOne, tokenId);
    expect(parseInt(event.logs[0].args.weiAmount)).toEqual(5);
  });

  it('should return 0 because franchisor withdraws before selling token', async () => {
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 5
    });
    event = await token.withdrawPayment(accountTwo, tokenId);
    expect(parseInt(event.logs[0].args.weiAmount)).toEqual(0);
  });
  it('should return 0 because franchisor tries to withdraw twice', async () => {
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 5
    });

    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 6
    });

    event = await token.withdrawPayment(accountTwo, tokenId);
    expect(parseInt(event.logs[0].args.weiAmount)).toEqual(3);
    event = await token.withdrawPayment(accountTwo, tokenId);
    expect(parseInt(event.logs[0].args.weiAmount)).toEqual(0);
  });

  it('should return the sum of half of all payments for minter', async () => {
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 5
    });

    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 6
    });
    accountOneWithdrawal = await token.withdrawPayment(accountOne, tokenId);
    expect(parseInt(accountOneWithdrawal.logs[0].args.weiAmount)).toEqual(8);
  });
});
