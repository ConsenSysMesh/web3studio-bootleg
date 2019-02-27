const BootlegToken = artifacts.require('BootlegToken');

module.exports = async deployer => {
  const tokenId = web3.utils.toBN(1);

  const accounts = await web3.eth.getAccounts();

  await deployer.deploy(BootlegToken, 'Bootleg', 'BLEG');

  const token = await BootlegToken.deployed();

  await token.mintWithTokenURI(
    accounts[0],
    tokenId,
    'https://ipfs.infura.io/ipfs/QmSomeHash'
  );
};
