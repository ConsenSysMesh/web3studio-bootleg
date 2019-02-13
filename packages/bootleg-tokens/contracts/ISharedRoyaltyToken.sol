pragma solidity ^0.5.3;

import "openzeppelin-solidity/contracts/token/ERC721/IERC721.sol";

/**
 * @title Shared Royalty Non-Fungible Token Standard basic interface
 */
contract ISharedRoyaltyToken is IERC721 {

  /**
   * @notice Gets the franchisor balance of the specified address
   *
   * @param _franchisor address to query the balance of
   * @return uint256 representing the amount franchised by the passed address
   */
  function franchisorBalanceOf(address _franchisor) public view returns (uint256 balance);

  /**
   * @notice Withdraws payment accumulated from transfer of a given token from
   *  the last withdrawn payment up to a _count
   *
   * @param _tokenId The identifier for an NFT
   * @param _count The number of payments to traverse
   */
  function withdrawPayment(uint256 _tokenId, uint256 _count) public;

  /**
   * @notice Withdraws non-withdrawn payment accumulated from transfer of a
   *   given token
   * @dev Escrow should keep track of the payments that have been processed to
   *   avoid double withdrawals
   *
   * @param _tokenId The identifier for an NFT
   */
  function withdrawPayment(uint256 _tokenId) public;

  /**
   * @notice Gets balance of non-withdrawn payment accumulated from transfer of a
   *   given token up to a number of provided payments
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   * @dev Implement this function to
   *
   * @param _tokenId The identifier for an NFT
   * @param _start The payment index to start from
   * @param _count The number of payments to traverse
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(uint256 _tokenId, uint256 _start, uint256 _count) public view returns (uint256);

  /**
   * @notice Gets balance of non-withdrawn payment accumulated from transfer of a
   *   given token that has not been withdrawn
   *
   * @dev Defaults to overloaded `withdrawPayment` of a token's non-withdrawn balance
   *   to the last payment
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   *
   * @param _tokenId The identifier for an NFT
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(uint256 _tokenId) public view returns (uint256);
}
