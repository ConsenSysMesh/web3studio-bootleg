pragma solidity ^0.5.0;

import "bootleg-tokens/contracts/BookendSharedRoyaltyToken.sol";
import "bootleg-tokens/contracts/ERC721/ERC721Enumerable.sol";
import "bootleg-tokens/contracts/ERC721/ERC721MetadataMintable.sol";
import "openzeppelin-solidity/contracts/access/roles/MinterRole.sol";

contract BootlegToken is ERC721MetadataMintable, BookendSharedRoyaltyToken, ERC721Enumerable {
  /**
   * @notice Create a new Bootleg Token Contract
   *
   * @param name - Name of the tokens created
   * @param symbol - Symbol of the tokens created
   */
  constructor (string memory name, string memory symbol, uint256 franchisorPercentage)
    ERC721Metadata(name, symbol) BookendSharedRoyaltyToken(franchisorPercentage) public
  {} // solium-disable-line no-empty-blocks


  /**
   * @notice Allow the minter to add franchisors. Used for assigning more than one initial position
   *
   * @param franchisor The new franchisor address
   * @param tokenId uint256 ID of the token that ownership is being passed to
   */
  function addFranchisor(address franchisor, uint256 tokenId) public payable onlyMinter {
    _addFranchisor(franchisor, msg.value, tokenId);
  }
}
