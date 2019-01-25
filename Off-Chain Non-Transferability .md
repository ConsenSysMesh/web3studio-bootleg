# Off-Chain Non-Transferability 
(snitches can't get stiches if the snitch is cryptography).
## Big Picture Explanation
To ensure that the pay-it-backward model maintains its integrity, we must de-incentivize people from sharing their video offchain. The method we will use is to force each reciever of a video to embed an invisible watermark into their video that is directly linked to their identity. If a copy  of a video is found in  unauthorized hands, it will be possible to decode the watermark, and find out who distributed it illicitly. 

The system will *force* users to embed watermark into their own copy of the video by first embedding the watermark into their decryption key. The user must use the decryption key to decrypt the video, but the result of the video won't exactly be the video. Perceptually, it will look and sound exatly the same as the video does, but it will actually contain the watermark.


## A little bit of math
Here is essentially how it works.
      
We have three look up tables.  
E: Encryption Look up table  
D: Decryption look up table  
W: Watermark look up table

A video is seen as a vector of values. We encrypt the vector by adding a randomly selected element from the encryption look up table to each component of the vector. The element chosen  is determined by a secret key.

If the video itself is labeled **x**, and the encrypted video is labeled **c** then:  
**c** = **x** + E.  
D = -E + W.   
**x** = **c** + D = **x** + E -  E + W = **x** + W (the watermarked version of  the video). 
  
The watermark table W  is generated from the users fingerprint, which is a value that  uniquely identifies the user. We use homomorphic encryption to allow the server to generate a watermark from the users fingerprint *without ever seeing the fingerprint*

The user sends the server  an encrypted  fingerprint, call it e(f). The server generates the watermark by operating on the encrypted fingerprint. Because the encryption scheme is homomorphic 2*e(a) = e(2*a). So, if the watermark will be the double of the fingerprint, the server can just double the *encryption* of the fingerprint, and the user will be able to decrypt it and find the intended watermark. 

The server can also generate the decryption table for the client, by encrypting each element of E with the client's public key, taking the inverse of this element. The encrypted inverse encryption table is added to the encrypted watermark and sent to the client. The client decrypts and obtains the correct decryption key. 

# Security
Security depends on the underlying encryption scheme, and the underlying **buyer/seller watermark protocol**. There is also a way to calculate the watermark that prevents collusions. 

#Performance
Looks reasonable - need to look into that more




