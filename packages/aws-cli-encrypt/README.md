# AWS CLI Encryption script

## Requirements

Ruby 2.6.2 - This project uses a `.ruby-version` file to manage the ruby version to use. Install/use rbenv with homebrew if you do not already have it installed.
Bundler - The Gemfile holds the list of gems needed to run this project. Run `gem install bundler` then `bundle install' to install the required gems. AWS CLI - Install the AWS CLI. Requires Python3 and pip3, consider installing this with`pyenv`(use`brew install pyenv`and`brew install pipenv`and then`pip3 install awscli --upgrade`)

## Installing

1. `brew install rbenv` - Run this to install rbenv with homebrew
1. `rbenv install` - From the project directory `packages/aws-cli-encrypt`, run this to install the correct version of Ruby
1. `gem install bundler` - Install bundler gem in your Ruby
1. `bundle install` - Install the project's required gems

## Setup

Run `aws configure` to setup your credentials for accessing AWS

## Usage

Encrypt a file
`./media-encrypt.rb -e gandalf.txt`

Output of this command will print the key used, save it. Keep it secret, keep it safe.

Decrypt a file
`./media-encrypt.rb -d gandalf.txt -k E1CqwJ5HLzn2SRwmicvWhy2uMiv3Ia3ZLrKI5D0k3IU=`

Output will print the contents of the file. This will need to be changed for large files/binaries/etc...

## Need help?

`./media-encrypt.rb -h` works or just run the script with no arguments

```bash
$ ./media-encrypt.rb -h
Usage ./media-encrypt.rb [options]
Uploads/downloads files with AWS client side encryption
    -e, --encrypt=FILEPATH           Encrypt and upload the file at the provided file path
    -d, --decrypt=S3PATH             Download and decrypt the file at the provided S3 path
    -b, --bucket=BUCKET              (optional) Bucket name for s3
    -k, --aes-key=AES_KEY            (optional) The AES encryption key to use
```

### Examples

Encrypting a file where we specify the bucket, key and file

```bash
$ ./media-encrypt.rb -e moby_dick.txt -b web3studio-moby-dick -k 4ZoMHtgYD1Snjks6S937D3tTW3L+IHurvSnkHghBogw=
-----------------------------------------------------
Key Used: 4ZoMHtgYD1Snjks6S937D3tTW3L+IHurvSnkHghBogw=
-----------------------------------------------------
(  ●   )  Uploading the file...
{:accept_ranges=>"bytes",
 :last_modified=>2019-03-21 17:21:52 +0000,
 :content_length=>12288,
 :etag=>"\"a27802c6cc40fee408ddf4595adb8d90\"",
 :content_type=>"",
 :metadata=>
  {"x-amz-key"=>
    "ZDQ6u5oR5m11La0SB6exIjRYgLN2cox+kCOupSbNaG14E8g7q6k5kymNAsZsquFd",
   "x-amz-unencrypted-content-length"=>"12286",
   "x-amz-matdesc"=>"{}",
   "x-amz-iv"=>"ilYTw0O1KupnoqlLPvv9xg=="}}
Put file to s3://web3studio-moby-dick/moby_dick.txt
```

Proof that it's encrypted at rest in S3...

```bash
$ aws s3 cp s3://web3studio-moby-dick/moby_dick.txt encrypted_moby.txt
$ head -c 100 encrypted_moby.txt
  # => Should output random bytes and be obviously not the text we uploaded
  ?U?ͣ?"P???ջ5?????$????~>y?<????&?PU?	?N???",7?:??G?E???W?2?M?;???X\hw??c???k*??w??7qk
```

vs. the original

```bash
$ $ head -c 100 moby_dick.txt
CHAPTER 1. Loomings.

Call me Ishmael. Some years ago—never mind how long precisely—having
```

Decrypting a file...

```bash
$ ./media-encrypt.rb -d moby_dick.txt -b web3studio-moby-dick -k 4ZoMHtgYD1Snjks6S937D3tTW3L+IHurvSnkHghBogw=
-----------------------------------------------------
Key Used: 4ZoMHtgYD1Snjks6S937D3tTW3L+IHurvSnkHghBogw=
-----------------------------------------------------
( ●    )  Downloading the file...
Wrote contents to 1553189368-moby_dick.txt
$ head -c 100 1553189368-moby_dick.txt
CHAPTER 1. Loomings.

Call me Ishmael. Some years ago—never mind how long precisely—having
```
