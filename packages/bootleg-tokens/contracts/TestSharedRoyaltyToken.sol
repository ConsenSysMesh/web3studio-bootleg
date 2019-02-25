pragma solidity ^0.5.0;

import "./AbstractSharedRoyaltyToken.sol";
import "./ERC721/ERC721Mintable.sol";

/**
 * @title A Test Shared Royalty Token implemented for testing purposes
 *
 * @dev You probably don't want to use this in any production form. It's an
 *  example and a basic way to test the Abstract token implementation
 */
contract TestSharedRoyaltyToken is AbstractSharedRoyaltyToken, ERC721Mintable {
  /**
   * @notice Gets remaining payment balance accumulated from transfer of a
   *   given token up to a number of provided payments
   * @dev Gives the payment to the person who sold it.
   *
   * @param franchisor address to get the payment balance of
   * @param start The payment index to start from
   * @param count The number of payments to traverse
   * @param tokenId The identifier for an NFT
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(address franchisor, uint256 start, uint256 count, uint256 tokenId) public view returns (uint256) {
    Token storage token = _tokens[tokenId];
    uint256 balance = 0;
    uint256 maxCount = start + count;

    maxCount = maxCount > token.payments.length ? token.payments.length : maxCount;

    for (uint256 i = start; i < maxCount; i += 1) {
      if (token.franchisors[i - 1] == franchisor) {
        balance += token.payments[i];
      }
    }

    return balance;
  }
}
