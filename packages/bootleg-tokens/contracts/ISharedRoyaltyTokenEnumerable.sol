pragma solidity 0.5.4;

/**
* @title Shared royalty NFT enumerable interface for franchisors, payments, and tokens
*/
contract ISharedRoyaltyTokenEnumerable {

  /**
  * @notice Gets the total number of franchisors given a token
  * @param _token The identifier of the NFT
  * @return uint256 The number of franchisors of that token
  */
  function tokenFranchisorsTotal(uint256 _token) public view returns (uint256 count);

  /**
  * @notice Enumerates the token franchisors for a given token
  * @dev Throws if `_index` >= `tokenFranchisorsTotal(_token)` or if
  *   `_token` is the zero address, representing an invalid NFT.
  * @param _index The index of the franchisor to return (should be less than total franchisors for the token)
  * @param _token The identifier of the NFT
  * @return address The address of the franchisor
  */
  function tokenFranchisorsByIndex(uint256 _index, uint256 _token) public view returns (address franchisor);

  /**
  * @notice Gets the total number of payments given a token
  * @param _token The identifier of the NFT
  * @return uint256 The number of payments made for the token
  */
  function tokenPaymentsTotal(uint256 _token) public view returns (uint256 count);

  /**
  * @notice Enumerates the token payments for a given token
  * @dev Throws if `_index` >= `tokenPaymentsTotal(_token)` or if
  *   `_token` is the zero address, representing invalid NFTs.
  * @param _index The index of the payment to return (should be less than total payments for the token)
  * @param _token The identifier of the NFT
  * @return unint256 The payment amount
  */
  function tokenPaymentsByIndex(uint256 _index, uint256 _token) public view returns (uint256 amount);

  /**
  * @notice Enumerates the NFTs assigned to a franchisor
  * @dev Throws if `_index` >= `franchisorBalanceOf(_franchisor)` or if
  *   `_franchisor` is the zero address, representing an invalid address.
  * @param _index The index of the token to return (should be less than total franchisor tokens)
  * @param _franchisor Address of the franchisor owner
  * @return uint256 The identifier of the NFT
  */
  function tokensOfFranchisorByIndex(uint256 _index, address _franchisor) public view returns (uint256 token);
}
