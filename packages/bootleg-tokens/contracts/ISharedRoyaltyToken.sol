pragma solidity ^0.5.3;

import "./ERC721/IERC721.sol";

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
   * @param _franchisor address to withdraw payment for
   * @param _count The number of payments to traverse
   */
  function withdrawPayment(uint256 _tokenId, address _franchisor, uint256 _count) public;

  /**
   * @notice Withdraws remaining payment balance accumulated from transfer of a
   *   given token
   * @dev Escrow should keep track of the payments that have been processed to
   *   avoid double withdrawals
   *
   * @param _tokenId The identifier for an NFT
   * @param _franchisor address to withdraw payment for
   */
  function withdrawPayment(uint256 _tokenId, address _franchisor) public;

  /**
   * @notice Gets remaining payment balance accumulated from transfer of a
   *   given token up to a number of provided payments
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   * @dev Every style of SharedRoyaltyToken will implement this function to create
   *   it's own royalty model
   *
   * @param _tokenId The identifier for an NFT
   * @param _franchisor address to get the payment balance of
   * @param _start The payment index to start from
   * @param _count The number of payments to traverse
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(uint256 _tokenId, address _franchisor, uint256 _start, uint256 _count) public view returns (uint256);

  /**
   * @notice Gets remaining payment balance accumulated from transfer of a
   *   given token that has not been withdrawn
   *
   * @dev Calls the overloaded `withdrawPayment` with defaults of `_start` as
   *  the last withdrawn payment index, and `_count` as the remaining payment
   *  length
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   *
   * @param _tokenId The identifier for an NFT
   * @param _franchisor address to get the payment balance of
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(uint256 _tokenId, address _franchisor) public view returns (uint256);
}
