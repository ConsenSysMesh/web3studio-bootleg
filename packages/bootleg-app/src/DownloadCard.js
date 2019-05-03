import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { Heading, Link, Text, Card, Box } from 'rimble-ui';

const DownloadCard = ({ accounts, drizzzleStatus, ...props }) => {
  return (
    <Box {...props}>
      <Card height={'100%'}>
        <Heading.h3 pb={3}>Download</Heading.h3>
        <Text>
          You are a franchisor and can{' '}
          <Link
            fontSize={'inherit'}
            href="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg.mp4"
            title="Download Video"
            download="outsideOUTSIDE-SXSW2019-Bootleg.mp4"
          >
            download
          </Link>{' '}
          the bootleg video
        </Text>
        <Text bold>
          Decryption key and instructions for the video file will be provided
          via email.
        </Text>
      </Card>
    </Box>
  );
};

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(DownloadCard, mapStateToProps);
