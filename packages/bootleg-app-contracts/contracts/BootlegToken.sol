pragma solidity ^0.5.0;

import "bootleg-tokens/contracts/BookendSharedRoyaltyToken.sol";
import "bootleg-tokens/contracts/ERC721/ERC721Enumerable.sol";
import "bootleg-tokens/contracts/ERC721/ERC721MetadataMintable.sol";
import "openzeppelin-solidity/contracts/access/roles/MinterRole.sol";

contract BootlegToken is ERC721MetadataMintable, BookendSharedRoyaltyToken, ERC721Enumerable {

  // Address of the application to trade this token
  address public traderApp;

  /**
   * @notice Create a new Bootleg Token Contract
   * @param name - Name of the tokens created
   * @param symbol - Symbol of the tokens created
   */
  constructor (string memory name, string memory symbol, uint256 franchisorPercentage)
    ERC721Metadata(name, symbol) BookendSharedRoyaltyToken(franchisorPercentage) public
  {} // solium-disable-line no-empty-blocks

  /**
   * @notice Allow the minter to add franchisors. Used for assigning more than one initial position
   * @param franchisor address The new franchisor address
   * @param tokenId uint256 ID of the token that ownership is being passed to
   */
  function addFranchisor(address franchisor, uint256 tokenId) public payable onlyMinter {
    _addFranchisor(franchisor, msg.value, tokenId);
  }

  /**
  * @notice Sets approved address/contract to trade this token on behalf of token owners.
  * @param traderAddr address The app contract that will trade/manage this token
  */
  function setTraderApp(address traderAddr) public onlyMinter {
    traderApp = traderAddr;
  }

  /**
  * @dev OVERRIDE from 721 token standard. Returns whether the given spender can transfer a given token ID.
  * @param spender address of the spender to query
  * @param tokenId uint256 ID of the token to be transferred
  * @return bool whether the msg.sender is approved for the given token ID,
  * is an operator of the owner, or is the owner of the token
  */
  function _isApprovedOrOwner(address spender, uint256 tokenId) internal view returns (bool) {
    address owner = ownerOf(tokenId);
    return (spender == traderApp || spender == owner);
  }
}
