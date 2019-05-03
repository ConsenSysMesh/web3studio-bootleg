const BootlegToken = artifacts.require('BootlegToken');
const BootlegTraderApp = artifacts.require('BootlegTraderApp');

const ipfsClient = require('ipfs-http-client');

const ipfsConfig = {
  host: 'ipfs.infura.io',
  port: 5001,
  options: {
    protocol: 'https'
  }
};

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

  // The artist ethereum wallet address
  const theArtist = process.env.ARTIST_ADDRESS || accounts[0];

  // The bootlegger ethereum wallet address
  const theBootlegger = process.env.BOOTLEGGER_ADDRESS || accounts[1];

  // Deploy our BootlegToken
  await deployer.deploy(BootlegToken, 'Bootleg', 'BLEG', 10);
  const token = await BootlegToken.deployed();

  // Deploy the trader app and pass in the token contract
  await deployer.deploy(BootlegTraderApp, token.address, tokenId);
  const app = await BootlegTraderApp.deployed();

  // Set the token to have the app as the only trader
  await token.setTraderApp(app.address);

  // The upload the json metadata defined by ERC721
  const path = './token_1_metadata.json';
  const ipfs = ipfsClient(ipfsConfig.host, ipfsConfig.port, ipfsConfig.options);
  const ipfsResult = await ipfs.addFromFs(path, { pin: true });

  // See 'metadata section' https://github.com/ethereum/EIPs/blob/master/EIPS/eip-721.md
  const metaDataURI = `https://ipfs.infura.io/ipfs/${ipfsResult[0].hash}`;

  // Do the minting with our metadata URI
  await token.mintWithTokenURI(theArtist, tokenId, metaDataURI);

  // Add Bootlegger as franchisor
  await token.addFranchisor(theBootlegger, tokenId);

  // Set this token up to be traded by the app contract
  await token.setTraderApp(app.address);
};
