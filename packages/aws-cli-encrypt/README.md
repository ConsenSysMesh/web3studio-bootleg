# AWS CLI Encryption script

## Requirements

Ruby 2.6.2 - This project uses a `.ruby-version` file to manage the ruby version to use. Install/use rbenv with homebrew if you do not already have it installed.
Bundler - The Gemfile holds the list of gems needed to run this project. Run `gem install bundler` then `bundle install' to install the required gems. AWS CLI - Install the AWS CLI. Requires Python3 and pip3, consider installing this with`pyenv`(use`brew install pyenv`and`brew install pipenv`and then`pip3 install awscli --upgrade`)

## Installing

`brew install rbenv` - Run this to install rbenv with homebrew
`rbenv install` - From the project directory `packages/aws-cli-encrypt`, run this to install the correct version of Ruby
`gem install bundler` - Install bundler gem in your Ruby
`bundle install` - Install the project's required gems

## Setup

Run `aws configure` to setup your credentials for accessing AWS

## Usage

Encrypt a file
`./media-encrypt.rb -e gandalf.txt`

Output of this command will print the key used, save it. Keep it secret, keep it safe.

Decrypt a file
`./media-encrypt.rb -d gandalf.txt -k E1CqwJ5HLzn2SRwmicvWhy2uMiv3Ia3ZLrKI5D0k3IU=`

Output will print the contents of the file. This will need to be changed for large files/binaries/etc...

### Examples

Encrypting a file...

```bash
$ ./media-encrypt.rb -e gandalf.txt -k E1CqwJ5HLzn2SRwmicvWhy2uMiv3Ia3ZLrKI5D0k6IU=
Uploading the file...
Used base64 encoded key E1CqwJ5HLzn2SRwmicvWhy2uMiv3Ia3ZLrKI5D0k6IU=
{:accept_ranges=>"bytes",
 :last_modified=>2019-03-21 02:30:37 +0000,
 :content_length=>96,
 :etag=>"\"2da6fa988d37043db948f8f2564d41ce\"",
 :content_type=>"",
 :metadata=>
  {"x-amz-key"=>
    "1NwVY5CMgxVb47LBeNOIl1HpTk0llwdutZP9MKU2Ph9uPr5IOrYjtHUFyK26PExx",
   "x-amz-unencrypted-content-length"=>"86",
   "x-amz-matdesc"=>"{}",
   "x-amz-iv"=>"0m0xGm2lJ9mANt73nGHyBA=="}}
Done!
```

Decrypting a file...

```bash
$ ./media-encrypt.rb -d gandalf.txt -k E1CqwJ5HLzn2SRwmicvWhy2uMiv3Ia3ZLrKI5D0k6IU=
Downloading the file...
'All we have to do is decide what to do with the time that is given to us.' - Gandalf
Done!
```
