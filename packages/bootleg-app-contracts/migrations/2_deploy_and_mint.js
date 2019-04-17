const BootlegToken = artifacts.require('BootlegToken');
const BootlegTraderApp = artifacts.require('BootlegTraderApp');

module.exports = async (deployer, network, accounts) => {
  /**
   * Note: This is an example deployment and migration script we created
   * for the purposes of trading the first ever BootlegToken. There are lots of
   * changes you would want to make if you were launching a BootlegToken for real.
   * For one, you would not want to hard-code the band or the bootlegger or the token ID.
   * You should build out your own dApp to handle the all the additional features you might
   * need to mint, trade, update, etc... your own Tokens.
   */

  // The Bootleg Token ID
  const tokenId = web3.utils.toBN(1);

  // Should be the Band
  const theArtist = process.env.BAND_ADDRESS || accounts[0];

  // Should be the Bootlegger
  const theBootlegger = process.env.BOOTLEGGER_ADDRESS || accounts[1];

  // The URL to the json metadata defined by ERC721
  // See 'metadata section' https://github.com/ethereum/EIPs/blob/master/EIPS/eip-721.md
  const metaDataURI = 'https://ipfs.infura.io/ipfs/QmSomeHash';

  // Deploy our BootlegToken
  await deployer.deploy(BootlegToken, 'Bootleg', 'BLEG', 25);
  const token = await BootlegToken.deployed();

  // Deploy the trader app and pass in the token contract
  await deployer.deploy(BootlegTraderApp, token.address, tokenId);
  const app = await BootlegTraderApp.deployed();

  // Set the token to have the app as the only trader
  await token.setTraderApp(app.address);

  // Do the minting with our metadata URI
  await token.mintWithTokenURI(theArtist, tokenId, metaDataURI);

  // Add Bootlegger as franchisor
  await token.addFranchisor(theBootlegger, tokenId);

  // Set this token up to be traded by the app contract
  await token.setTraderApp(app.address);
};
