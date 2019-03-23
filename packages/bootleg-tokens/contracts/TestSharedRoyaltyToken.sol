pragma solidity ^0.5.0;

import "./ERC721/ERC721Mintable.sol";
import "./BookendSharedRoyaltyToken.sol";

/**
 * @title A Test Shared Royalty Token implemented for testing purposes
 *
 * @dev You probably don't want to use this in any production form. It's an
 *  example and a basic way to test the Abstract token implementation
 */
contract TestSharedRoyaltyToken is BookendSharedRoyaltyToken, ERC721Mintable {

  constructor (uint256 franchisorPercentage)
    BookendSharedRoyaltyToken(franchisorPercentage) public
  {} // solium-disable-line no-empty-blocks
}
