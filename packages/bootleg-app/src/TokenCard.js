import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { ContractData } from 'drizzle-react-components';
import { Card } from 'rimble-ui';

const TokenCard = ({ accounts, drizzleStatus }) => (
  <Card>
    <div
      style={{
        display: 'block',
        overflowWrap: 'break-word',
        width: '850px'
      }}
    >
      <img
        style={{
          paddingRight: '1em',
          width: '350px',
          height: '200px',
          float: 'left'
        }}
        alt="Bootleg Token Cover Art"
        src="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg-cover.png"
      />
      <span style={{ fontSize: '2em' }}>Token #1</span>
      <br />
      <span style={{ fontSize: '2em' }}>outsideOUTSIDE SXSW2019 Show</span>
      <p style={{ fontSize: '1.2em' }}>
        Recording of the artist outsideOUTSIDE at the Consensys SXSW Blockchain
        house. Captured March 13th, 2019 @ 8PM.{' '}
        <a href="https://www.outsideoutside.band">
          https://www.outsideoutside.band
        </a>
      </p>
      <p>
        Owned by:
        <ContractData contract="BootlegTraderApp" method="getOwner" />
      </p>
    </div>
  </Card>
);

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(TokenCard, mapStateToProps);
