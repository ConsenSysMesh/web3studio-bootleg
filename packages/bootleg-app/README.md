<h1 align="center">
  <br/>
  <a href='https://github.com/ConsenSys/web3studio-bootleg'><img
      width='250px'
      alt='Bootleg'
      src="https://user-images.githubusercontent.com/5770007/52348724-02aa0780-29f3-11e9-9039-71880d1af2b6.png" /></a>
  <br/>
  Trading App UI
  <br/>
</h1>

<h4 align="center">
 Bootleg Trading dApp Web UI
</h4>

<p align="center">
  <a href="#license">License</a>
</p>

A reference implementation for a Bootleg web dApp.

## Ganache Accounts

```
Available Accounts
==================
(0) 0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1 (~100 ETH)
(1) 0xffcf8fdee72ac11b5c542428b35eef5769c409f0 (~100 ETH)
(2) 0x22d491bde2303f2f43325b2108d26f1eaba1e32b (~100 ETH)
(3) 0xe11ba2b4d45eaed5996cd0823791e0c93114882d (~100 ETH)
(4) 0xd03ea8624c8c5987235048901fb614fdca89b117 (~100 ETH)
(5) 0x95ced938f7991cd0dfcb48f0a06a40fa1af46ebc (~100 ETH)
(6) 0x3e5e9111ae8eb78fe1cc3bb8915d5d461f3ef9a9 (~100 ETH)
(7) 0x28a8746e75304c0780e011bed21c72cd78cd535e (~100 ETH)
(8) 0xaca94ef8bd5ffee41947b4585a84bda5a3d3da6e (~100 ETH)
(9) 0x1df62f291b2e969fb0849d99d9ce41e2f137006e (~100 ETH)

Private Keys
==================
(0) 0x4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d
(1) 0x6cbed15c793ce57650b9877cf6fa156fbef513c4e6134f022a85b1ffdd59b2a1
(2) 0x6370fd033278c143179d81c5526140625662b8daa446c22ee2d73db3707e620c
(3) 0x646f1ce2fdad0e6deeeb5c7e8e5543bdde65e86029e2fd9fc169899c440a7913
(4) 0xadd53f9a7e588d003326d1cbf9e4a43c061aadd9bc938c843a79e7b4fd2ad743
(5) 0x395df67f0c2d2d9fe1ad08d1bc8b6627011959b79c53d7dd6a3536a33ab8a4fd
(6) 0xe485d098507f54e7733a205420dfddbe58db035fa577fc294ebd14db90767a52
(7) 0xa453611d9419d0e56f499079478fd72c37b251a94bfde4d19872c44cf65386e3
(8) 0x829e924fdf021ba3dbbc4225edfece9aca04b929d6e75613329ca6f1d31c0bb4
(9) 0xb0057716d5917badaf911b193b12b910811c1497b5bada8d7711f758981c3773
```

<br/>

## Bootleg file encryption steps

For this demo app the [dONT](https://github.com/ConsenSys/web3studio-dONT) was not ready.
Instead we created a used AES 256 symmetric encryption. The steps are below:

### Prerequisites

You need to have OpenSSL installed and on the system path.

### Encrypting

Encrypt the file using the OpenSSL toolkit. Your password should be 32 random characters. You can generate
one with the command below.

```bash
openssl rand -base64 32
```

```bash
openssl aes-256-cbc -salt -in outsideOUTSIDE-SXSW2019-Bootleg.raw.mp4 -out outsideOUTSIDE-SXSW2019-Bootleg.enc -k <PASSWORD_GOES_HERE>
```

### Uploading

Upload the file to the existing s3 bucket

```bash
aws s3 cp outsideOUTSIDE-SXSW2019-Bootleg.enc s3://web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg.mp4
```

### Tagging to make it public

We followed [this method of tagging s3 objects](https://aws.amazon.com/premiumsupport/knowledge-center/read-access-objects-s3-bucket/) to make the media files for the token publicly accessible.

From the command line you can set the tag like this.

```
aws s3api put-object-tagging --bucket web3studio-bootlegs --key outsideOUTSIDE-SXSW2019-Bootleg.mp4 --tagging '{"TagSet": [{"Key":"public","Value":"yes"}]}'
```

## License

[Apache 2.0](LICENSE)
