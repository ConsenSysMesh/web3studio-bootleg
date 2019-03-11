# dont-crypto

This is the directory where the dont cryptography implementation will live

To use:
In main directory do:
`npm install`
`stack build`

To test, do
`serverless offline start`

http://localhost:3000/getintarray will show an int array (in next commit, it will be retrieving a value from DynamoDB. )
http://localhost:3000/getrejected will show {"message":"unauthorized"} because requests go through a not-yet-working authorization function
