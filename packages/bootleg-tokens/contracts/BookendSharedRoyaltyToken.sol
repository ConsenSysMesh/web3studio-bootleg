pragma solidity ^0.5.3;


import "./AbstractSharedRoyaltyToken.sol"; 
import "./ERC721/ERC721Mintable.sol";
/**
 * @title Shared Royalty Non-Fungible Token Standard basic interface
 */

contract BookendSharedRoyaltyToken is AbstractSharedRoyaltyToken, ERC721Mintable {
  uint256 franchisorPercentage;   
  constructor(uint256 _franchisorPercentage) public {
    franchisorPercentage = _franchisorPercentage;
  }
  /**
   * @notice Calculates the balance owned to the given franchisor, using
  ***the bookend model, where only the first franchisor and the last
  ***franchisor get anything. In this implementation, they each get
  ***50% of the sale.
   *
   * @param franchisor request withdrawal
   * @param start the last franchisor that requested withdrawal
   * @param count the number of payments left for this franchisor
   * @param tokenId - id of the token whose payment is being requested
  */
  function paymentBalanceOf(address franchisor,uint256 start,uint256 count,uint256 tokenId) public view returns (uint256) {    
    Token storage token = _tokens[tokenId];
    uint256 maxCount = start + count;

    // guard for payments array overrun
    maxCount = maxCount > token.payments.length ? token.payments.length : maxCount;

    uint256 payment = 0;
    // if we are getting payment balance for the first franchisor
    // do this first because they get (100 - franchisorPercentage) amount
    if (franchisor == _tokens[tokenId].franchisors[0]) {
      for (uint256 i = start; i < maxCount; i++) {
        payment += (token.payments[i] * (100 - franchisorPercentage))/100;
      }
    } 
    // For everyone
    for (uint256 i = start; i < maxCount; i++) {
      if (token.franchisors[i - 1] == franchisor) {
        payment += (token.payments[i] * franchisorPercentage)/100;
        // If the ??? modulo 100 ???
        if ((token.payments[i] * franchisorPercentage) % 100 != 0){
          payment += 1;
        }
      }
    }

    return payment;
  }
}
