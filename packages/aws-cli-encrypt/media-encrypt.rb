#!/usr/bin/env ruby
$stderr.sync = true

# Ruby libraries
require 'openssl'
require 'pp'
require 'optparse'

# Bundler gems
require 'rubygems'
require 'bundler/setup'
Bundler.require(:default)

CYPHER_TYPE = 'AES-256-ECB'
KEY_SIZE = 32

defaults = {
  bucket: 'web3studio-dev-bootlegs',
  aes_key: OpenSSL::Cipher.new(CYPHER_TYPE).random_key # symmetric key
}

options = {}
optParser = OptionParser.new do |opts|
  opts.banner = "Usage #{__FILE__} [options] \nUploads/downloads files with AWS clientside encryption"

  opts.on("-e", "--encrypt=FILEPATH", "Encrypt and upload the file at the provided file path") do |filepath|
    raise RuntimeError, "Conflicting args provided" unless options[:action].nil?
    options[:action] = :encrypt
    options[:path] = filepath
  end

  opts.on("-d", "--decrypt=S3PATH", "Download and decrypt the file at the provided S3 path") do |s3path|
    raise RuntimeError, "Conflicting args provided" unless options[:action].nil?
    options[:action] = :decrypt
    options[:path] = s3path
  end

  opts.on("-b", "--bucket=BUCKET", "(optional) Provide the S3 bucket name for uploads") do |bucket|
    options[:bucket] = bucket
  end

  opts.on("-k", "--aes-key=AES_KEY", "(optional) Provide the AES encryption key to use (base64 encoded)") do |aes_key|
    key = aes_key.unpack('m')[0]
    raise RuntimeError, "Key Error! Wrong size key for #{CYPHER_TYPE}" if(key.nil? or key.size != KEY_SIZE)
    options[:aes_key] = key
  end
end

optParser.parse!

# Print the help if no options are passed in
optParser.parse(%w[--help]) if options.empty?

class MediaHandler

  attr_accessor :base_client
  attr_accessor :encrypt_client

  def self.process_options(options)
    handler = MediaHandler.new(aes_key: options[:aes_key])

    action = options.delete(:action)
    case action
    when :encrypt
      puts "Uploading the file..."
      handler.encrypt_upload(options)
      puts "Used base64 encoded key #{[options[:aes_key]].pack('m')}"
      resp = handler.get_metadata(bucket: options[:bucket], path: options[:path])
      pp resp.to_h

    when :decrypt
      puts "Downloading the file..."
      file_contents = handler.decrypt_download(options)
      puts file_contents

    else
      # nothing
    end

    puts 'Done!'
  end

  def initialize(aes_key:)
    @base_client = Aws::S3::Client.new(region: 'us-east-1')
    @encrypt_client = Aws::S3::Encryption::Client.new(encryption_key: aes_key, client: @base_client)
  end

  def decrypt_download(bucket:, path:, aes_key:)
    @encrypt_client.get_object(bucket: bucket, key: path).body.read
  end

  def get_metadata(bucket:, path:)
    @base_client.head_object(bucket: bucket, key: path)
  end

  def encrypt_upload(bucket:, path:, aes_key:)
    @base_client.create_bucket(bucket: bucket) # is a no-op if it exists

    s3path = File.basename(path)
    file_contents = File.read(path)
    @encrypt_client.put_object(bucket: bucket, key: s3path, body: file_contents)
  end
end

MediaHandler.process_options(defaults.merge(options)) unless options.empty?