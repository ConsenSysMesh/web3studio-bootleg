import BootlegTraderApp from './contracts/BootlegTraderApp';
import BootlegToken from './contracts/BootlegToken';
import Web3 from 'web3';

const options = {
  web3: {
    // TODO: test this without using the custom RPC in metamask
    customProvider: new Web3.providers.WebsocketProvider('ws://127.0.0.1:7545')
  },
  contracts: [BootlegTraderApp(), BootlegToken()],
  events: {},
  polls: {
    accounts: 1500
  }
};

export default options;
