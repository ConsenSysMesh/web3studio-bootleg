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

Then the decryption look up table is  
D = -E + W. 
  
The watermark tabke W  is generated from the users fingerprint, which is a value that  uniquely identifies them. 





