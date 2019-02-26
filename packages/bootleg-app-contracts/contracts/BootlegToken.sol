pragma solidity ^0.5.0;

import "bootleg-tokens/contracts/AbstractSharedRoyaltyToken.sol";
import "bootleg-tokens/contracts/ERC721/ERC721Enumerable.sol";
import "bootleg-tokens/contracts/ERC721/ERC721MetadataMintable.sol";
import "openzeppelin-solidity/contracts/access/roles/MinterRole.sol";

contract BootlegToken is ERC721MetadataMintable, AbstractSharedRoyaltyToken, ERC721Enumerable {
  /**
   * @dev Constructor function
   */
  constructor (string memory name, string memory symbol) ERC721Metadata(name, symbol) public {}

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
