import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { ContractData } from 'drizzle-react-components';
import { Text, Blockie } from 'rimble-ui';

const TokenDetails = ({ accounts, drizzleStatus }) => (
  <>
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
    <Text.span style={{ fontSize: '3em' }}>Token #1</Text.span>
    <br />
    <Text.span style={{ fontSize: '2em' }}>
      outsideOUTSIDE SXSW2019 Show
    </Text.span>
    <p style={{ fontSize: '1.2em' }}>
      Recording of the artist outsideOUTSIDE at the Consensys SXSW Blockchain
      house. Captured March 13th, 2019 @ 8PM.{' '}
      <a href="https://www.outsideoutside.band">
        https://www.outsideoutside.band
      </a>
    </p>
    <p>
      <ContractData
        contract="BootlegTraderApp"
        method="getOwner"
        render={ownerAddress => (
          <>
            Owned by:
            <br />
            <Blockie
              opts={{
                seed: ownerAddress,
                color: '#dfe',
                bgcolor: '#a71',
                size: 15,
                scale: 3,
                spotcolor: '#000'
              }}
            />
            <Text.span> {ownerAddress}</Text.span>
          </>
        )}
      />
    </p>
    {/* <p>
      <ContractData 
        contract="BootlegTraderApp"
        method="totalFranchisors"
        render={totalFranchisors => (
          <Text.span>Total Franchisors: {totalFranchisors}</Text.span>
        )}
        />
    </p>
    <p>
      <ContractData 
        contract="BootlegTraderApp"
        method="total Payments"
        render={totalPayments=> (
          <Text.span>Total Payments: {totalPayments}</Text.span>
        )}
        />
    </p> */}
  </>
);

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(TokenDetails, mapStateToProps);
