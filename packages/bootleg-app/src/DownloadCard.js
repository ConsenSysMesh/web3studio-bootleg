import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { Card, Link } from 'rimble-ui';

const DownloadCard = ({ accounts, drizzzleStatus }) => {
  return (
    <Card>
      Download the video from this link:{' '}
      <Link
        href="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg.mp4"
        title="Download Video"
        download="outsideOUTSIDE-SXSW2019-Bootleg.mp4"
      >
        https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg.mp4
      </Link>
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
