# Helix Issues
### A list of End to End impelemntations that build on each other
1. **Goal**:Provide access to encrypted video with access control (so only to authorized people)
   * **Server Side**:  
        1. Encrypt video with AES in CBC mode, so only one artifact is needed for decryption 
        2. Retrieve public keys of all token owners from smart contract
        3. Client will send an API request that includes a signature. Server verifies that the signature came from the correct private key
        4. Upon verification, server sends location of video, along with private key (both items are encrypted with users public key)
   * **Client Side**:
   		1. Send API request to server that includes an ECDSA signature with Ethereum private key.
   		2. Fetch and decrypt video
2. **Goal**:Provide watermarked and encrypted video with access control. **The following will be added/changed from the above**:
	* **Server Side**
		1.  Encrypt video with AES in output feedback mode, so extra decryption key is also needed in addition to the private key.
		2. Create a fingerprint and watermark for each client, and use it to generate the decryption key
		3. Save hash of fingerprint to smart contract       
	* **Client Side**
      1.  Decrypt video for AES in output feedback mode
3. **Goal**: Provide *client side* watermarking of encrypted video. **The following will be added/changed from the above**
	*  **Server Side**
		1. Receive encrypted fingerprint from client instead of creating one
		2. Instead of saving hash to smart contract, save encrypted fingerprint to smart contract
	3. **Client Side**
		1. Generate, encrypt, and send fingerprint 
4. **Goal**: The above scenarios require us to trust that the server will send the correct decryption key after the server recieves payment. Some on-chain escrow type service would be included here to verify the transaction is honest. Method to be determined...
5. **Goal**: In the 3rd scenario, once the fingerprint is decoded, it will have to be encrypted with every possible persons public key, to see who it matches with. This is inefficient. It would be better if a hash of the fingerprint were saved. But in order to make this possible, a proof must be included that the encrypted fingerprint and the hashed fingerpring have matching inputs. 
	* **Server Side**: Verify encrypted fingerprint is the same as hashed fingerprint
	* **Client Side**: Send a hash of the fingerprint, along with a proof that it is the same as the encrypted fingerprint
6. Actually decode an encrypted video to find a fingerprint. This would be server side code.


 
