import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData, ContractForm } from 'drizzle-react-components';
import { Form, Button, Card } from 'rimble-ui';

const BalanceCard = ({ accounts, drizzzleStatus }, { drizzle }) => {
  const { web3 } = drizzle;

  return (
    <Card>
      <ContractData
        contract="BootlegTraderApp"
        method="getBalance"
        render={tokenPrice => (
          <>
            <h2>
              Your Bootleg royalty earnings this token:{' '}
              {web3.utils.fromWei(tokenPrice, 'ether')} Eth
            </h2>
            <p>
              This is the balance you have in payments owed to you from
              franchise royalties.
            </p>
          </>
        )}
      />
      <ContractForm
        contract="BootlegTraderApp"
        method="withdraw"
        render={({
          inputs,
          inputTypes,
          state,
          handleInputChange,
          handleSubmit
        }) => (
          <Form onSubmit={handleSubmit}>
            <Button
              key="submit"
              type="button"
              onClick={handleSubmit}
              size={'medium'}
            >
              Withdraw
            </Button>
          </Form>
        )}
      />
    </Card>
  );
};

BalanceCard.contextTypes = { drizzle: PropTypes.object };

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(BalanceCard, mapStateToProps);
