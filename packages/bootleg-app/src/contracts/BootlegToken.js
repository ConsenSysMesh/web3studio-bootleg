export default () => {
  let networks = {
    // MainNet
    '1': {},
    // Rinkeby
    '4': {
      address: '0x19671109381e6bF2001D0394c7F040A2C367Bc77',
      transactionHash:
        '0x0b64e473896141444cb8e28262df4a08345a0e43780b8c48aba345e3d23e19ab'
    }
  };

  if (process.env.NODE_ENV !== 'production') {
    const {
      networks: devNetworks
    } = require('../build/contracts/BootlegToken');

    networks = {
      ...networks,
      ...devNetworks
    };
  }

  return {
    networks,
    contractName: 'BootlegToken',
    abi: [
      {
        constant: true,
        inputs: [
          {
            name: 'interfaceId',
            type: 'bytes4'
          }
        ],
        name: 'supportsInterface',
        outputs: [
          {
            name: '',
            type: 'bool'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x01ffc9a7'
      },
      {
        constant: true,
        inputs: [],
        name: 'name',
        outputs: [
          {
            name: '',
            type: 'string'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x06fdde03'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'getApproved',
        outputs: [
          {
            name: '',
            type: 'address'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x081812fc'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'to',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'approve',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x095ea7b3'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'withdrawPayment',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x0e2cdba7'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          },
          {
            name: 'start',
            type: 'uint256'
          },
          {
            name: 'count',
            type: 'uint256'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'paymentBalanceOf',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x1383bfc7'
      },
      {
        constant: true,
        inputs: [],
        name: 'totalSupply',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x18160ddd'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'from',
            type: 'address'
          },
          {
            name: 'to',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'transferFrom',
        outputs: [],
        payable: true,
        stateMutability: 'payable',
        type: 'function',
        signature: '0x23b872dd'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'owner',
            type: 'address'
          },
          {
            name: 'index',
            type: 'uint256'
          }
        ],
        name: 'tokenOfOwnerByIndex',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x2f745c59'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          },
          {
            name: 'count',
            type: 'uint256'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'withdrawPayment',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x3c800161'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'index',
            type: 'uint256'
          }
        ],
        name: 'tokenByIndex',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x4f6ccce7'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'to',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          },
          {
            name: 'tokenURI',
            type: 'string'
          }
        ],
        name: 'mintWithTokenURI',
        outputs: [
          {
            name: '',
            type: 'bool'
          }
        ],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x50bb4e7f'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'ownerOf',
        outputs: [
          {
            name: '',
            type: 'address'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x6352211e'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'owner',
            type: 'address'
          }
        ],
        name: 'balanceOf',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x70a08231'
      },
      {
        constant: true,
        inputs: [],
        name: '_INTERFACE_ID_SRT',
        outputs: [
          {
            name: '',
            type: 'bytes4'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x86abecf5'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'franchisorWithdrawPaymentsLeft',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x8970c257'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'paymentBalanceOf',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x919c3db5'
      },
      {
        constant: true,
        inputs: [],
        name: 'symbol',
        outputs: [
          {
            name: '',
            type: 'string'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0x95d89b41'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'account',
            type: 'address'
          }
        ],
        name: 'addMinter',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x983b2d56'
      },
      {
        constant: false,
        inputs: [],
        name: 'renounceMinter',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x98650275'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'to',
            type: 'address'
          },
          {
            name: 'approved',
            type: 'bool'
          }
        ],
        name: 'setApprovalForAll',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0xa22cb465'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'account',
            type: 'address'
          }
        ],
        name: 'isMinter',
        outputs: [
          {
            name: '',
            type: 'bool'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0xaa271e1a'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'from',
            type: 'address'
          },
          {
            name: 'to',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          },
          {
            name: '_data',
            type: 'bytes'
          }
        ],
        name: 'safeTransferFrom',
        outputs: [],
        payable: true,
        stateMutability: 'payable',
        type: 'function',
        signature: '0xb88d4fde'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'tokenURI',
        outputs: [
          {
            name: '',
            type: 'string'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0xc87b56dd'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          }
        ],
        name: 'franchisorBalanceOf',
        outputs: [
          {
            name: '',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0xd840fd8c'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'owner',
            type: 'address'
          },
          {
            name: 'operator',
            type: 'address'
          }
        ],
        name: 'isApprovedForAll',
        outputs: [
          {
            name: '',
            type: 'bool'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0xe985e9c5'
      },
      {
        constant: true,
        inputs: [],
        name: 'traderApp',
        outputs: [
          {
            name: '',
            type: 'address'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0xf84908a2'
      },
      {
        inputs: [
          {
            name: 'name',
            type: 'string'
          },
          {
            name: 'symbol',
            type: 'string'
          },
          {
            name: 'franchisorPercentage',
            type: 'uint256'
          }
        ],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'constructor',
        signature: 'constructor'
      },
      {
        anonymous: false,
        inputs: [
          {
            indexed: true,
            name: 'payee',
            type: 'address'
          },
          {
            indexed: false,
            name: 'weiAmount',
            type: 'uint256'
          }
        ],
        name: 'WithdrawnPayment',
        type: 'event',
        signature:
          '0x3d3ab7d78e99c30b0f84aa86c09e959872657e970a59b0148095a22c52a5c940'
      },
      {
        anonymous: false,
        inputs: [
          {
            indexed: true,
            name: 'account',
            type: 'address'
          }
        ],
        name: 'MinterAdded',
        type: 'event',
        signature:
          '0x6ae172837ea30b801fbfcdd4108aa1d5bf8ff775444fd70256b44e6bf3dfc3f6'
      },
      {
        anonymous: false,
        inputs: [
          {
            indexed: true,
            name: 'account',
            type: 'address'
          }
        ],
        name: 'MinterRemoved',
        type: 'event',
        signature:
          '0xe94479a9f7e1952cc78f2d6baab678adc1b772d936c6583def489e524cb66692'
      },
      {
        anonymous: false,
        inputs: [
          {
            indexed: true,
            name: 'from',
            type: 'address'
          },
          {
            indexed: true,
            name: 'to',
            type: 'address'
          },
          {
            indexed: true,
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'Transfer',
        type: 'event',
        signature:
          '0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef'
      },
      {
        anonymous: false,
        inputs: [
          {
            indexed: true,
            name: 'owner',
            type: 'address'
          },
          {
            indexed: true,
            name: 'approved',
            type: 'address'
          },
          {
            indexed: true,
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'Approval',
        type: 'event',
        signature:
          '0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925'
      },
      {
        anonymous: false,
        inputs: [
          {
            indexed: true,
            name: 'owner',
            type: 'address'
          },
          {
            indexed: true,
            name: 'operator',
            type: 'address'
          },
          {
            indexed: false,
            name: 'approved',
            type: 'bool'
          }
        ],
        name: 'ApprovalForAll',
        type: 'event',
        signature:
          '0x17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'franchisor',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'addFranchisor',
        outputs: [],
        payable: true,
        stateMutability: 'payable',
        type: 'function',
        signature: '0xbcb927b6'
      },
      {
        constant: false,
        inputs: [
          {
            name: 'traderAddr',
            type: 'address'
          }
        ],
        name: 'setTraderApp',
        outputs: [],
        payable: false,
        stateMutability: 'nonpayable',
        type: 'function',
        signature: '0x2ac35da3'
      },
      {
        constant: true,
        inputs: [
          {
            name: 'possibleFranchisor',
            type: 'address'
          },
          {
            name: 'tokenId',
            type: 'uint256'
          }
        ],
        name: 'isTokenFranchisor',
        outputs: [
          {
            name: '',
            type: 'bool'
          }
        ],
        payable: false,
        stateMutability: 'view',
        type: 'function',
        signature: '0xa2378bdf'
      }
    ]
  };
};
