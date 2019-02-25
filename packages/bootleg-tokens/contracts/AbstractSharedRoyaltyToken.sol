pragma solidity ^0.5.0;

import "./ERC721/ERC721.sol";
import "./ISharedRoyaltyToken.sol";

/**
 * @title Shared Royalty Non-Fungible Token Standard basic interface
 */
contract AbstractSharedRoyaltyToken is ISharedRoyaltyToken, ERC721 {

  event WithdrawnPayment(address indexed payee, uint256 weiAmount);

  struct Token {
    address[] franchisors;
    uint256[] payments;
    mapping(address => uint256) franchisorNextWithdrawIndex;
  }

  bytes4 public constant _INTERFACE_ID_SRT = 0x68f3a538;

  /**
   * 0x68f3a538 ==
   *   bytes4(keccak256('franchisorBalanceOf(address)')) ^
   *   bytes4(keccak256('withdrawPayment(address,uint256,uint256)')) ^
   *   bytes4(keccak256('withdrawPayment(address,uint256)')) ^
   *   bytes4(keccak256('paymentBalanceOf(address,uint256,uint256,uint256)')) ^
   *   bytes4(keccak256('paymentBalanceOf(address,uint256)'));
   **/

  // Mapping from tokenId to a Token
  mapping (uint256 => Token) internal _tokens;

  // Mapping from franchisor to number of franchised tokens
  mapping (address => uint256) private _franchisorTokensCount;

  constructor () public {
    // register the supported interfaces to conform to ERC721 via ERC165
    _registerInterface(_INTERFACE_ID_SRT);
  }

  /**
   * @notice Transfers the ownership of a given token ID to another address
   *   Usage of this method is discouraged, use `safeTransferFrom` whenever possible
   *   Requires the msg sender to be the owner, approved, or operator
   *
   * @dev Only overriding this function as both safeTransferFrom functions
   *   call this one
   *
   * @param from current owner of the token
   * @param to address to receive the ownership of the given token ID
   * @param tokenId uint256 ID of the token to be transferred
   */
  function transferFrom(address from, address to, uint256 tokenId) public payable {
    super.transferFrom(from, to, tokenId);

    _addFranchisor(to, msg.value, tokenId);
  }

  /**
   * @notice Gets the franchisor balance of the specified address
   *
   * @param franchisor address to query the balance of
   * @return uint256 representing the amount franchised by the passed address
   */
  function franchisorBalanceOf(address franchisor) public view notZeroAddr(franchisor) returns (uint256) {
    return _franchisorTokensCount[franchisor];
  }

  /**
   * @notice Withdraws payment accumulated from transfer of a given token from
   *  the last withdrawn payment up to a count
   *
   * @dev if count is greater than the total number of payments left to withdraw,
   *  the function will only withdraw up to the payments left
   *
   * @param tokenId The identifier for an NFT
   * @param franchisor address to withdraw payment for
   * @param count The number of payments to traverse
   */
  function withdrawPayment(address payable franchisor, uint256 count, uint256 tokenId) public notZeroAddr(franchisor) {
    Token storage token = _tokens[tokenId];

    uint256 withdrawPaymentsLeft = franchisorWithdrawPaymentsLeft(franchisor, tokenId);
    uint256 lastWithdrawnIndex = token.franchisorNextWithdrawIndex[franchisor];
    uint256 withdrawCount = lastWithdrawnIndex.add(count) > token.payments.length
      ? withdrawPaymentsLeft
      : count;

    uint256 balance = paymentBalanceOf(
      franchisor,
      token.franchisorNextWithdrawIndex[franchisor],
      withdrawCount,
      tokenId
    );

    token.franchisorNextWithdrawIndex[franchisor] = lastWithdrawnIndex.add(withdrawCount);

    franchisor.transfer(balance);

    emit WithdrawnPayment(franchisor, balance);
  }

  /**
   * @notice Withdraws remaining payment balance accumulated from transfer of a
   *   given token
   * @dev Escrow should keep track of the payments that have been processed to
   *   avoid double withdrawals
   *
   * @param tokenId The identifier for an NFT
   * @param franchisor address to withdraw payment for
   */
  function withdrawPayment(address payable franchisor, uint256 tokenId) public notZeroAddr(franchisor) {
    withdrawPayment(
      franchisor,
      franchisorWithdrawPaymentsLeft(franchisor, tokenId),
      tokenId
    );
  }

  /**
   * @notice Gets remaining payment balance accumulated from transfer of a
   *   given token that has not been withdrawn
   *
   * @dev Calls the overloaded `withdrawPayment` with defaults of `start` as
   *  the last withdrawn payment index, and `count` as the remaining payment
   *  length
   * @dev Used by `withdrawPayment` to calculate how much a franchisor is owed.
   *
   * @param tokenId The identifier for an NFT
   * @param franchisor address to get the payment balance of
   *
   * @return uint256 representing the balance in wei accumulated for a token
   */
  function paymentBalanceOf(address franchisor, uint256 tokenId)
    public view notZeroAddr(franchisor) returns (uint256)
  {
    Token storage token = _tokens[tokenId];

    return paymentBalanceOf(
      franchisor,
      token.franchisorNextWithdrawIndex[franchisor],
      franchisorWithdrawPaymentsLeft(franchisor, tokenId),
      tokenId
    );
  }

  /**
   * @notice Gets the number of payments left to withdraw for a franchisor
   *
   * @param franchisor address to query the balance of
   * @param tokenId The identifier for an NFT
   * @return uint256 representing the amount of payments left to withdraw
   */
  function franchisorWithdrawPaymentsLeft(address franchisor, uint256 tokenId)
    public view returns (uint256)
  {
    Token storage token = _tokens[tokenId];

    require(_isFranchisor(franchisor, tokenId), "Payments can only be withdrawn by franchisors");

    return token.payments.length.sub(token.franchisorNextWithdrawIndex[franchisor]);
  }

  /**
   * @notice Internal function to mint a new token
   *   Reverts if the given token ID already exists
   *
   * @param to The address that will own the minted token
   * @param tokenId uint256 ID of the token to be minted
   */
  function _mint(address to, uint256 tokenId) internal {
    super._mint(to, tokenId);

    _addFranchisor(to, 0, tokenId);
  }

  /**
   * @notice Internal function add a franchisor
   *
   * @param franchisor The new franchisor address
   * @param payment The payment collected to add this new franchisor
   * @param tokenId uint256 ID of the token that ownership is being passed to
   */
  function _addFranchisor(address franchisor, uint256 payment, uint256 tokenId) internal {
    Token storage token = _tokens[tokenId];

    token.franchisors.push(franchisor);
    token.payments.push(payment);
    _franchisorTokensCount[franchisor] = _franchisorTokensCount[franchisor].add(1);

    if (!_isFranchisor(franchisor, tokenId)) {
      token.franchisorNextWithdrawIndex[franchisor] = token.payments.length;
    }
  }

  /**
   * @notice Internal function to check if an account is a franchisor of a token
   *
   * @param franchisor The franchisor address
   * @param tokenId uint256 ID of the token to check
   */
  function _isFranchisor(address franchisor, uint256 tokenId) internal view returns(bool) {
    Token storage token = _tokens[tokenId];

    return token.franchisorNextWithdrawIndex[franchisor] > 0;
  }

  /**
   * @notice Modifier to ensure an address isn't 0x0
   *
   * @param account The account that shouldn't be 0
   */
  modifier notZeroAddr(address account) {
    require(account != address(0), "Can't be used with the 0x0 address");
    _;
  }
}
