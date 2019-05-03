import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { ContractData } from 'drizzle-react-components';
import { Flex, Box, Text, Image, Link, Blockie } from 'rimble-ui';

const TokenDetails = ({ accounts, drizzleStatus }) => (
  <Flex>
    <Box p={3}>
      <Image
        style={{
          width: '350px',
          height: '200px'
        }}
        alt="Bootleg Token Cover Art"
        src="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg-cover.png"
      />
    </Box>
    <Box p={2}>
      <Text.p style={{ fontSize: '3em' }}>Token #1</Text.p>
      <Text.p style={{ fontSize: '2em' }}>outsideOUTSIDE SXSW2019 Show</Text.p>
      <Text.p style={{ fontSize: '1.2em' }}>
        Recording of the artist outsideOUTSIDE at the Consensys SXSW Blockchain
        house. Captured March 13th, 2019 @ 8PM.{' '}
        <Link href="https://www.outsideoutside.band">
          https://www.outsideoutside.band
        </Link>
      </Text.p>
      <Box>
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
      </Box>
    </Box>
  </Flex>
);

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(TokenDetails, mapStateToProps);
