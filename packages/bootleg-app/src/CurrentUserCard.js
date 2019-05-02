import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { AccountData } from 'drizzle-react-components';
import { Card } from 'rimble-ui';

const CurrentUserCard = ({ accounts, drizzleStatus }) => (
  <Card fontSize={4}>
    <span>You are logged in with the following account:</span>
    <AccountData
      accountIndex="0"
      units="ether"
      precision="3"
      render={({ address, balance, units }) => (
        <div>
          <div>
            Address: <span style={{ color: 'red' }}>{address}</span>
          </div>
          <div>
            Balance: <span style={{ color: 'red' }}>{balance}</span> {units}
          </div>
        </div>
      )}
    />
  </Card>
);

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(CurrentUserCard, mapStateToProps);
