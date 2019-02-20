pragma solidity ^0.5.3;

/**
* @title Shared Royalty NFT Enumerable interface for franchisors
*/
contract ISharedRoyaltyTokenEnumerable {

  /**
  * @notice Gets the total number of franchisors given a Token ID
  * @param _tokenId The identifier of the NFT
  * @return uint256 The number of franchisors
  */
  function tokenFranchisorsTotal(uint256 _tokenId) public view returns (uint256 count);

  /**
  * @notice Gets the token franchisor for a given token at a specific index
  * @param _tokenId The identifier of the NFT
  * @param _index The index (should be less than total franchisors for the token)
  * @return address The address of the franchisor
  */
  function tokenFranchisorsByIndex(uint256 _index, uint256 _tokenId) public view returns (address owner);

  /**
  * @notice Gets the total number of payments given a Token ID
  * @param _tokenId The identifier of the NFT
  * @return uint256 The number of payments
  */
  function tokenPaymentsTotal(uint256 _tokenId) public view returns (uint256 count);

  /**
  * @notice Gets the token payment for a given token at a specific index
  * @param _tokenId The identifier of the NFT
  * @param _index The index (should be less than total payments for the token)
  * @return unint256 The payment amount
  */
  function tokenPaymentsByIndex(uint256 _index, uint256 _tokenId) public view returns (uint256 amount);
}
