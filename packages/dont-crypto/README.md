
<h1 align="center">
The dONT (decentralized Off-chain Non-transferable) system
  <br/>
</h1>

<h4 align="center">
Library for encrypting and storing a video; generating decryption keys that contain identity-based watermarks; decrypting a video with a decryption protocol that embeds a watermark into the video; decodes video to find identity of person for whom it was intended
</h4>

**Still WIP**

To use:
In main directory do:
`npm install`
`stack build`

To test, do
`serverless offline start`

http://localhost:3000/getintarray will show an int array (in next commit, it will be retrieving a value from DynamoDB. )
http://localhost:3000/getrejected will show {"message":"unauthorized"} because requests go through a not-yet-working authorization function




