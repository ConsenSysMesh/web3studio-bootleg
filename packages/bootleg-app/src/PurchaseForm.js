import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData, ContractForm } from 'drizzle-react-components';
import { Form, Button } from 'rimble-ui';

const PurchaseForm = ({ accounts, drizzzleStatus }, { drizzle }) => {
  const { web3 } = drizzle;

  return (
    <ContractData
      contract="BootlegTraderApp"
      method="tokenPrice"
      render={tokenPrice =>
        tokenPrice == 0 ? (
          <h2>Not currently for sale</h2>
        ) : (
          <>
            <h2>
              Current price is: {web3.utils.fromWei(tokenPrice, 'ether')} Eth
            </h2>
            <ContractForm
              contract="BootlegTraderApp"
              method="purchase"
              sendArgs={{ value: tokenPrice }}
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
                    Purchase
                  </Button>
                </Form>
              )}
            />
          </>
        )
      }
    />
  );
};

PurchaseForm.contextTypes = { drizzle: PropTypes.object };

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(PurchaseForm, mapStateToProps);
