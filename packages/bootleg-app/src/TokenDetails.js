import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { Flex, Box, Text, Heading, Link } from 'rimble-ui';

const TokenDetails = ({ children }) => (
  <>
    <Flex alignItems={'center'} py={2}>
      <Box width={[1, 1 / 2, 7 / 12]}>
        <Heading.h2>outsideOUTSIDE SXSW2019 Show</Heading.h2>
        <Text>
          Recording of the artist{' '}
          <Link href="https://www.outsideoutside.band">outsideOUTSIDEM</Link> at
          the <Text.span bold>Consensys SXSW Blockchain House</Text.span>.
        </Text>
        <Text>Captured March 13th, 2019 @ 8PM.</Text>
      </Box>

      <Box py={3} width={[1, 1 / 2, 5 / 12]}>
        {' '}
        {children}
      </Box>
    </Flex>
  </>
);

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(TokenDetails, mapStateToProps);
