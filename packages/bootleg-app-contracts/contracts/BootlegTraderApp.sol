pragma solidity ^0.5.0;

import "./BootlegToken.sol";

contract BootlegTraderApp {

  BootlegToken public bootlegToken;
  mapping(uint256 => uint256) private _tokenPrices;
  
  event Purchased(address newOwner, uint256 tokenPrice);
  event PriceChanged(address changer, uint256 oldPrice, uint256 newPrice);

  /**
   * @notice Create a new Bootleg Token trading app
   *
   * @param deployedAddress - Deployed address of the BootlegToken contract 
   */
  constructor (address deployedAddress) public {
    bootlegToken = BootlegToken(deployedAddress);
  }

  /**
  * @notice Provides a way to become a franchisor a Bootleg token for a fee.
  * Emits a Purchased event. 
  * @param tokenId - uint256 The token Id to purchase
  * @param newOwner - address The address of the new owner/franchisor
  */
  function purchase(uint256 tokenId, address newOwner) public payable {
    uint256 tokenPrice = _tokenPrices[tokenId];

    require(tokenPrice > 0, "Token must have non-zero price before it can be franchised");
    require(msg.value == tokenPrice, "Must provide eth to franchise the token");

    //bootlegToken.addFranchisor.value(msg.sender).(newOwner, tokenId);
    address owner = bootlegToken.ownerOf(tokenId);
    bootlegToken.safeTransferFrom.value(msg.value)(owner, newOwner, tokenId, "");

    emit Purchased(newOwner, tokenPrice);
  }

  /**
  * @notice Gets the token price given a tokenId
  * @param tokenId - uint256 The token Id to query for price
  * @return uint256 - the token price in Wei
   */
  function getTokenPrice(uint256 tokenId) public view returns (uint256) {
    return _tokenPrices[tokenId];
  }

  /**
  * @notice Sets the token price given a tokenId
  * Emits a PriceChanged event
  * @param tokenId - uint256 The token Id to set the price
  * @param newPrice - uint256 The new token price in Wei
   */
  function setTokenPrice(uint256 tokenId, uint256 newPrice) public {
    address currentOwner = bootlegToken.ownerOf(tokenId);
    require(msg.sender == currentOwner, "Only the owner can change the token price");
    uint256 oldPrice = _tokenPrices[tokenId];
    _tokenPrices[tokenId] = newPrice;

    emit PriceChanged(msg.sender, oldPrice, newPrice);
  }
}
