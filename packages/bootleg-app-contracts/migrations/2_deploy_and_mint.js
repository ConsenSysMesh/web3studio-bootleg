const BootlegToken = artifacts.require('BootlegToken');
const BootlegTraderApp = artifacts.require('BootlegTraderApp');

module.exports = async (deployer, network, accounts) => {

  let firstOwner = web3.utils.toHex(0);
  let metaDataURI = 'https://ipfs.infura.io/ipfs/QmSomeHash';

  // TOKEN NUMERO UNO!!!
  const tokenId = web3.utils.toBN(1);

  if (network == "rinkeby") {
    firstOwner = '0x61D87e60B3893801DbC9056c10bc124251Ea7980'
  }
  else {
    firstOwner = accounts[0];
  }
  // Deploy our BootlegToken
  await deployer.deploy(BootlegToken, 'Bootleg', 'BLEG', 20);
  const token = await BootlegToken.deployed();

  await deployer.deploy(BootlegTraderApp, token.address);
  const app = await BootlegTraderApp.deployed();

  // Do the minting
  await token.mintWithTokenURI(
    firstOwner,
    tokenId,
    metaDataURI
  );
  
  // Set this token up to be traded by the app contract
  await token.approve(app.address, tokenId);
};