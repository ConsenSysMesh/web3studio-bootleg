import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { Card, Link, Text } from 'rimble-ui';

const DownloadCard = ({ accounts, drizzzleStatus }) => {
  return (
    <Card>
      <h2>
        Congratulations! You are a franchisor and can access the bootleg video
      </h2>
      Download the video from this link:{' '}
      <Link
        href="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg.mp4"
        title="Download Video"
        download="outsideOUTSIDE-SXSW2019-Bootleg.mp4"
      >
        https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg.mp4
      </Link>
      <Text.p style={{ fontWeight: 'bold' }}>
        Decryption key for video file will be provided via email.
      </Text.p>
    </Card>
  );
};

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(DownloadCard, mapStateToProps);
