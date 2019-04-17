const BootlegToken = artifacts.require('BootlegToken');
const BootlegTraderApp = artifacts.require('BootlegTraderApp');

module.exports = async (deployer, network, accounts) => {
  let firstOwner = process.env.BAND_ADDRESS || web3.utils.toHex(0);
  const metaDataURI = 'https://ipfs.infura.io/ipfs/QmSomeHash';

  // Bootleg Token ID
  const tokenId = web3.utils.toBN(1);
  // should be the Band
  firstOwner = accounts[0];
  // should be the Bootlegger
  secondOwner = accounts[1];

  // Deploy our BootlegToken
  await deployer.deploy(BootlegToken, 'Bootleg', 'BLEG', 25);
  const token = await BootlegToken.deployed();

  // Deploy the trader app and pass in the token contract
  await deployer.deploy(BootlegTraderApp, token.address, tokenId);
  const app = await BootlegTraderApp.deployed();

  // Set the token to have the app as the only trader
  await token.setTraderApp(app.address);

  // Do the minting
  await token.mintWithTokenURI(firstOwner, tokenId, metaDataURI);

  // Add franchisor
  await token.addFranchisor(secondOwner, tokenId);

  // Set this token up to be traded by the app contract
  await token.setTraderApp(app.address);
};
