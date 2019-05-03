import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData, ContractForm } from 'drizzle-react-components';
import { Form, Button, Card, Text, Heading } from 'rimble-ui';

const BalanceCard = ({ accounts, drizzzleStatus }, { drizzle }) => {
  const { web3 } = drizzle;

  return (
    <Card>
      <ContractData
        contract="BootlegTraderApp"
        method="getBalance"
        render={royalties =>
          royalties >= web3.utils.toBN(0) ? (
            <>
              <Heading.h2>
                Your Bootleg royalty earnings on this token:{' '}
                {web3.utils.fromWei(royalties, 'ether')} Eth
                <Text.p style={{ color: 'red' }}>in Wei:{royalties}</Text.p>
              </Heading.h2>
              <Text.p>
                This is the balance you have in payments owed to you from
                franchise royalties.
              </Text.p>
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
            </>
          ) : (
            <Heading.h2>No royalties earned yet.</Heading.h2>
          )
        }
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
