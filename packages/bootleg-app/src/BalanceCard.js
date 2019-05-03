import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData, ContractForm } from 'drizzle-react-components';
import { Form, Button, Card, Text, Heading, Box } from 'rimble-ui';

const BalanceCard = ({ accounts, drizzzleStatus, ...props }, { drizzle }) => {
  const { web3 } = drizzle;

  return (
    <ContractData
      contract="BootlegTraderApp"
      method="getBalance"
      render={royalties =>
        royalties >= web3.utils.toBN(0) ? (
          <Box {...props}>
            <Card height={'100%'}>
              <Heading.h3 pb={3}>Withdraw</Heading.h3>
              <Text>
                After you sell the bootleg, you can withdraw your balance here.
              </Text>
              <Text pt={2}>
                Current Balance: {web3.utils.fromWei(royalties, 'ether')}
              </Text>
              {royalties > 0 ? (
                <ContractForm
                  contract="BootlegTraderApp"
                  method="withdraw"
                  render={({ handleSubmit }) => (
                    <Form onSubmit={handleSubmit}>
                      <Button
                        mt={3}
                        key="submit"
                        type="button"
                        onClick={handleSubmit}
                        size={'medium'}
                      >
                        Withdraw balance
                      </Button>
                    </Form>
                  )}
                />
              ) : null}
            </Card>
          </Box>
        ) : (
          <Heading.h2>No royalties earned yet.</Heading.h2>
        )
      }
    />
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
