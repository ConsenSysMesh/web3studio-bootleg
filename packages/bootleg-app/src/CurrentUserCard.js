import React from 'react';
import { AccountData } from 'drizzle-react-components';
import { Box, Text } from 'rimble-ui';

const CurrentUserCard = () => (
  <Box fontSize={4} pt={4}>
    <Text>Logged in as:</Text>
    <AccountData
      accountIndex={0}
      units="ether"
      precision={3}
      render={({ address, balance, units }) => (
        <>
          <Text>
            Address: <span style={{ color: '#4E3FCE' }}>{address}</span>
          </Text>
          <Text>
            Balance: <span style={{ color: '#4E3FCE' }}>{balance}</span> {units}
          </Text>
        </>
      )}
    />
  </Box>
);

export default CurrentUserCard;
