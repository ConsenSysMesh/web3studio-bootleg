const expect = require('jest-matchers');
const BookendSharedRoyaltyToken = artifacts.require(
  'BookendSharedRoyaltyToken'
);

contract('BookendSharedRoyaltyToken', accounts => {
  const accountOne = accounts[0];
  const accountTwo = accounts[1];
  const accountThree = accounts[2];
  const tokenId = 1;

  //count is less than payment.length
  it('count->minter->franchisor->integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    const balance = await token.paymentBalanceOf(accountOne, 1, 1, tokenId);
    expect(parseInt(balance)).toEqual(10);
  }); //minter -> franchisor -> payment is not integer

  it('count -> minter -> franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 7
    });
    const balance = await token.paymentBalanceOf(accountOne, 1, 1, tokenId);
    expect(parseInt(balance)).toEqual(7);
  });

  it('count -> minter -> non franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.withdrawPayment(accountOne, 1, tokenId);
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 10
    });
    const balance = await token.paymentBalanceOf(accountOne, 2, 1, tokenId);
    expect(parseInt(balance)).toEqual(9);
  });

  it('count -> minter -> non franchisor -> integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.withdrawPayment(accountOne, 1, tokenId);
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 100
    });
    const balance = await token.paymentBalanceOf(accountOne, 2, 1, tokenId);
    expect(parseInt(balance)).toEqual(95);
  });

  it('count -> non-minter -> franchisor -> integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 100
    });
    const balance = await token.paymentBalanceOf(accountTwo, 2, 1, tokenId);
    expect(parseInt(balance)).toEqual(5);
  });

  it('count -> non-minter -> franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 101
    });
    const balance = await token.paymentBalanceOf(accountTwo, 2, 1, tokenId);
    expect(parseInt(balance)).toEqual(6);
  });

  it('count -> non-minter -> non-franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 101
    });
    const balance = await token.paymentBalanceOf(accountThree, 3, 1, tokenId);
    expect(parseInt(balance)).toEqual(0);
  });

  it('count -> non-minter -> non-franchisor -> integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 100
    });
    const balance = await token.paymentBalanceOf(accountThree, 3, 1, tokenId);
    expect(parseInt(balance)).toEqual(0);
  });

  it('countTooBig->minter->franchisor->integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    const balance = await token.paymentBalanceOf(accountOne, 1, 5, tokenId);
    expect(parseInt(balance)).toEqual(10);
  }); //minter -> franchisor -> payment is not integer

  it('countTooBig -> minter -> franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 7
    });
    const balance = await token.paymentBalanceOf(accountOne, 1, 5, tokenId);
    expect(parseInt(balance)).toEqual(7);
  });
  it('countTooBig -> minter -> non franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.withdrawPayment(accountOne, 1, tokenId);
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 10
    });
    const balance = await token.paymentBalanceOf(accountOne, 2, 5, tokenId);
    expect(parseInt(balance)).toEqual(9);
  });

  it('countTooBig -> minter -> non franchisor -> integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.withdrawPayment(accountOne, 1, tokenId);
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 100
    });
    const balance = await token.paymentBalanceOf(accountOne, 2, 5, tokenId);
    expect(parseInt(balance)).toEqual(95);
  });

  it('countTooBig -> non-minter -> franchisor -> integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 100
    });
    const balance = await token.paymentBalanceOf(accountTwo, 2, 5, tokenId);
    expect(parseInt(balance)).toEqual(5);
  });

  it('countTooBig -> non-minter -> franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 101
    });
    const balance = await token.paymentBalanceOf(accountTwo, 2, 5, tokenId);
    expect(parseInt(balance)).toEqual(6);
  });

  it('countTooBig -> non-minter -> non-franchisor -> non-integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 101
    });
    const balance = await token.paymentBalanceOf(accountThree, 3, 5, tokenId);
    expect(parseInt(balance)).toEqual(0);
  });

  it('countTooBig -> non-minter -> non-franchisor -> integer', async () => {
    const token = await BookendSharedRoyaltyToken.new(5);
    await token.mint(accountOne, tokenId);
    await token.transferFrom(accountOne, accountTwo, tokenId, {
      value: 10
    });
    await token.transferFrom(accountTwo, accountThree, tokenId, {
      from: accountTwo,
      value: 100
    });
    const balance = await token.paymentBalanceOf(accountThree, 3, 5, tokenId);
    expect(parseInt(balance)).toEqual(0);
  });
});
