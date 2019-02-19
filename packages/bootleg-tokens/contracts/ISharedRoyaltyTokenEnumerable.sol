pragma solidity ^0.5.3;

/**
 * @title Shared Royalty NFT Enumerable interface for franchisors
 */
contract ISharedRoyaltyTokenEnumerable {

    function totalSupply() public view returns (uint256);
    function tokenOfOwnerByIndex(address owner, uint256 index) public view returns (uint256 tokenId);

    function tokenByIndex(uint256 index) public view returns (uint256);

    /**
    * @notice Gets the total number of Franchisees given a Token ID
    * @param _tokenId The identifier of the NFT
    * @return uint256 The number of franchisees
    */
    function tokenFranchiseesTotal(uint256 _tokenId) public view returns (uint256 count);

    /**
    * @notice Gets the token franchisee for a given token at a specific index
    * @param _tokenId The identifier of the NFT
    * @param _index The index, should be less than total franchisees for the token
    * @return address The address of the franchisee
    */
    function tokenFranchiseesByIndex(uint256 _tokenId, uint256 _index) public view returns (address owner);

    /**
    * @notice Utility function gets the payout in eth for the token and franchisee at a given index for a given amount.
    * @dev Implement the payment model here based on your distribution design (e.g. bookend, linear, weighted curve, etc..)
    * @param _tokenId The identifier of the NFT
    * @param _index The index, should be less than total franchisees for the token
    * @param _amount The ammount, in eth, total sale price for the token
    */
    function calculatePayoutOfFranchiseeByIndex(uint256 _tokenId, uint256 _index, uint256 _amount) public view returns (uint256 payout);
}