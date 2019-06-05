pragma solidity 0.5.4;

import "./BootlegToken.sol";

contract BootlegTraderApp {

  // The bootleg token contract
  BootlegToken public bootlegToken;
  // Only trading one token for this dApp, this is the ID for the token
  uint256 public bootlegTokenId;
  // Only one token traded only requires one price to store
  uint256 public tokenPrice;

  event Purchased(address newOwner, uint256 weiAmount);
  event PriceChanged(address changer, uint256 oldPriceInWei, uint256 newPriceInWei);
  event PaymentWithdrawn(address withdrawer);

  /**
   * @notice Create a new Bootleg Token trading app
   *
   * @param deployedAddress address Deployed address of the BootlegToken contract
   * @param tokenId uint256 TokenID to trade with this app
   */
  constructor (address deployedAddress, uint256 tokenId) public {
    bootlegToken = BootlegToken(deployedAddress);
    bootlegTokenId = tokenId;
  }

  /**
  * @notice Provides a way to become a franchisor a Bootleg token for a fee
  * Emits a Purchased event
  */
  function purchase() public payable {
    require(tokenPrice > 0, "Token must have non-zero price before it can be franchised");
    require(msg.value == tokenPrice, "Must provide eth to franchise the token");

    address newOwner = msg.sender;
    address owner = bootlegToken.ownerOf(bootlegTokenId);
    // Do the token transfer
    bootlegToken.safeTransferFrom.value(msg.value)(owner, newOwner, bootlegTokenId, "");
    // set the price to zero to prevent purchase
    setTokenPrice(0);

    emit Purchased(newOwner, tokenPrice);
  }

  /**
  * @notice Sets the token price given a tokenId
  * Emits a PriceChanged event
  * @param newPrice uint256 The new token price in Wei
   */
  function setTokenPrice(uint256 newPrice) public {
    address currentOwner = bootlegToken.ownerOf(bootlegTokenId);
    // TODO: Or allow minter to do this
    require(msg.sender == currentOwner, "Only the owner can change the token price");
    uint256 oldPrice = tokenPrice;
    tokenPrice = newPrice;

    emit PriceChanged(msg.sender, oldPrice, newPrice);
  }

  /**
  * @notice Gets current token owner
  * @return address Address of current owner
  */
  function getOwner() public view returns (address) {
    return bootlegToken.ownerOf(bootlegTokenId);
  }

  /**
  * @notice Gets current payment balance of your token payments
  * @return uint256 Payment balance in Wei
  */
  function getBalance() public view returns (uint256) {
    return bootlegToken.paymentBalanceOf(msg.sender, bootlegTokenId);
  }

  /**
  * @notice Use to widthraws eth from the payments you recieved during franchising
  * Emits a PaymentWithdrawn event
  * @dev Calls the withdrawPayment function on the BootlegToken
  */
  function withdraw() public {
    bootlegToken.withdrawPayment(msg.sender, bootlegTokenId);

    emit PaymentWithdrawn(msg.sender);
  }

  /**
  * @notice  Gets the metadataURI for the token
  * @return string token URI
  */
  function getTokenURI() public view returns (string memory) {
    return bootlegToken.tokenURI(bootlegTokenId);
  }

  /**
  * @notice Returns true/false if the address provided is for a franchisor
  * @param possibleFranchisor address The address of the account
  * @return bool
  */
  function isTokenFranchisor(address possibleFranchisor) public view returns (bool) {
    return bootlegToken.isTokenFranchisor(possibleFranchisor, bootlegTokenId);
  }

}
