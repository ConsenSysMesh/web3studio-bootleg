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
  function franchisorBalanceOf(address _franchisor) public view returns (uint256);

  /**
   * @notice Gets the number of payments left to withdraw for a franchisor
   *
   * @param _franchisor address to query the balance of
   * @param _tokenId The identifier for an NFT
   * @return uint256 representing the amount of payments left to withdraw
   */
  function franchisorWithdrawPaymentsLeft(address _franchisor, uint256 _tokenId) public view returns (uint256);

  /**
   * @notice Withdraws payment accumulated from transfer of a given token from
   *  the last withdrawn payment up to a _count
   *
   * @param _franchisor address to withdraw payment for
   * @param _count The number of payments to traverse
   * @param _tokenId The identifier for an NFT
   */
  function withdrawPayment(address payable _franchisor, uint256 _count, uint256 _tokenId) public;

  /**
   * @notice Withdraws remaining payment balance accumulated from transfer of a
   *   given token
   * @dev Escrow should keep track of the payments that have been processed to
   *   avoid double withdrawals
   *
   * @param _franchisor address to withdraw payment for
   * @param _tokenId The identifier for an NFT
   */
  function withdrawPayment(address payable _franchisor, uint256 _tokenId) public;

  /**
   * @notice Gets remaining payment balance accumulated from transfer of a
   *   given token up to a number of provided payments
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   * @dev Every style of SharedRoyaltyToken will implement this function to create
   *   it's own royalty model
   *
   * @param _franchisor address to get the payment balance of
   * @param _start The payment index to start from
   * @param _count The number of payments to traverse
   * @param _tokenId The identifier for an NFT
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(address _franchisor, uint256 _start, uint256 _count, uint256 _tokenId) public view returns (uint256);

  /**
   * @notice Gets remaining payment balance accumulated from transfer of a
   *   given token that has not been withdrawn
   *
   * @dev Calls the overloaded `withdrawPayment` with defaults of `_start` as
   *  the last withdrawn payment index, and `_count` as the remaining payment
   *  length
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   *
   * @param _franchisor address to get the payment balance of
   * @param _tokenId The identifier for an NFT
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(address _franchisor, uint256 _tokenId) public view returns (uint256);
}
