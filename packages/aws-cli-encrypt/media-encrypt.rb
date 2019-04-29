#!/usr/bin/env ruby
$stderr.sync = true

# Ruby libraries
require 'openssl'
require 'pp'
require 'optparse'

# Bundler gems
require 'rubygems'
require 'bundler/setup'
# Import all the gems in the Gemfile
Bundler.require(:default)

CYPHER_TYPE = 'AES-256-ECB'.freeze
KEY_SIZE = 32

# Processes the file according to the options provided
class MediaHandler
  attr_accessor :base_client
  attr_accessor :encrypt_client
  attr_accessor :bucket

  # Initialize the Media Handler object with the AES key and bucket
  # Note: Creates the bucket in S3 (if it does not exist)
  #
  # @param aes_key [String, #read] the AES encryption key in base64 encoding
  # @param bucket [String] the S3 bucket name
  def initialize(aes_key:, bucket:)
    @base_client = Aws::S3::Client.new(region: 'us-east-1')
    self.aes_key_base64 = aes_key
    @encrypt_client = Aws::S3::Encryption::Client.new(
      encryption_key: @aes_key,
      client: @base_client
    )
    @bucket = bucket
    @base_client.create_bucket(bucket: @bucket) # is a no-op if it exists
  end

  # Helper to get metadata on the uploaded file.
  # Used to check the clientside encryption key usage
  #
  # @param path [String] the S3 path (aka "key") to the file
  # @return [Hash] the head object response as a hash
  def get_metadata(path)
    resp = @base_client.head_object(bucket: @bucket, key: path)
    resp.to_h
  end

  # Read the file into memory and then
  # upload file in one shot. Okay for small files (< 100MB)
  #
  # @param path [String] the filesystem path
  # @return [Types::PutObjectOutput] response object
  def encrypt_upload(path)
    s3path = File.basename(path)
    file_contents = File.read(path)
    @encrypt_client.put_object(bucket: @bucket, key: s3path, body: file_contents)
  end

  # Upload file in chuncks, better performance for large files
  # as they do not need to be read into memory first.
  #
  # @param path [String] the filesystem path
  # @return [Types::PutObjectOutput] response object
  def encrypt_stream(path)
    s3path = File.basename(path)
    File.open(path, 'rb') do |file|
      @encrypt_client.put_object(bucket: @bucket, key: s3path, body: file)
    end
  end

  # Download the file in one shot and return the contents
  #
  # @param path [String] the S3 key path
  # @return [String] decrypted file contents
  def decrypt_download(path)
    @encrypt_client.get_object(bucket: @bucket, key: path).body.read
  end

  # Stream download the file in chucks an write to disk
  # Yields a block to the caller to handle the chuck writing
  #
  # @param path [String] the s3 path
  # @yieldparam chunk [IO] the file chunk to be written
  def decrypt_stream(path)
    @encrypt_client.get_object(bucket: @bucket, key: path) do |chunk|
      yield chunk
    end
  end

  # Converts the provide base64 encoded key to standard encoding
  #
  # @param [String] base64 encoded key string
  # @return [String] binary packed key string
  def aes_key_base64=(key)
    @aes_key = key.unpack1('m')
    raise "Key Error! Wrong size key for #{CYPHER_TYPE}" if @aes_key.nil? || @aes_key.size != KEY_SIZE
  end

  # Returns the AES key in base64 encoding
  #
  # @return [String] base64 encoded AES key
  def aes_key_base64
    [@aes_key].pack('m')
  end
end

options = {}
opt_parser = OptionParser.new do |opts|
  opts.banner = "Usage #{__FILE__} [options] \nUploads/downloads files with AWS client side encryption"

  opts.on('-e', '--encrypt=FILEPATH', 'Encrypt and upload the file at the provided file path') do |filepath|
    raise 'Conflicting args provided' unless options[:action].nil?

    options[:action] = :encrypt
    options[:path] = filepath
  end

  opts.on('-d', '--decrypt=S3PATH', 'Download and decrypt the file at the provided S3 path') do |s3path|
    raise 'Conflicting args provided' unless options[:action].nil?

    options[:action] = :decrypt
    options[:path] = s3path
  end

  opts.on('-b', '--bucket=BUCKET', '(optional) Provide the S3 bucket name for uploads') do |bucket|
    options[:bucket] = bucket
  end

  opts.on('-k', '--aes-key=AES_KEY', '(optional) Provide the AES encryption key to use (base64 encoded)') do |aes_key|
    options[:aes_key] = aes_key
  end
end

opt_parser.parse!

# Print the help if no options are passed
if options.empty?
  opt_parser.parse(%w[--help])
  exit
end

# Some defaults
options = {
  bucket: 'web3studio-dev-bootlegs',
  aes_key: [OpenSSL::Cipher.new(CYPHER_TYPE).random_key].pack('m') # symmetric key
}.merge(options)

handler = MediaHandler.new(aes_key: options[:aes_key], bucket: options[:bucket])

# Wrap the spinner for convenience
def whirly_wrap(action)
  Whirly.start(spinner: 'bouncingBall')
  Whirly.status = "#{action} the file..."
  yield
  Whirly.stop
end

case options.delete(:action)
when :encrypt
  whirly_wrap('Uploading') do
    handler.encrypt_stream(options[:path])
  end
  
  s3path = File.basename(options[:path])
  puts "Put file to s3://#{options[:bucket]}/#{s3path}"

  # The user should probably take note of this
  # since the encryption key is not stored
  puts '-----------------------------------------------------'
  puts "Key Used: #{handler.aes_key_base64}"
  puts '-----------------------------------------------------'

  # Checking metadata on the file 
  pp handler.get_metadata(s3path)

when :decrypt
  file_name = Time.now.to_i.to_s + '-' + File.basename(options[:path])
  whirly_wrap('Downloading') do
    File.open(file_name, 'wb') do |f|
      handler.decrypt_stream(options[:path]) do |chunk|
        f.write(chunk)
      end
    end
  end
  puts "Wrote contents to #{file_name}"

end
