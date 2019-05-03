import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractForm, ContractData } from 'drizzle-react-components';
import { Form, Button, Input, Heading } from 'rimble-ui';

const SellForm = ({ accounts, drizzzleStatus }, { drizzle }) => {
  const { web3, contracts } = drizzle;

  const handleSetTokenPriceSubmit = (inputs, state) => event => {
    event.preventDefault();

    const convertedInputs = inputs.map(input => {
      if (input.type === 'bytes32') {
        return web3.utils.toHex(state[input.name]);
      }
      if (input.name === 'newPrice') {
        return web3.utils.toWei(state[input.name], 'ether');
      }
      return state[input.name];
    });
    return contracts.BootlegTraderApp.methods.setTokenPrice.cacheSend(
      ...convertedInputs
    );
  };

  return (
    <>
      <ContractData
        contract="BootlegTraderApp"
        method="tokenPrice"
        render={tokenPrice =>
          tokenPrice <= web3.utils.toBN(0) ? (
            <Heading.h2>
              Not currently for sale. Set price below to list for sale{' '}
            </Heading.h2>
          ) : (
            <Heading.h2>
              Current price is: {web3.utils.fromWei(tokenPrice, 'ether')} Eth
            </Heading.h2>
          )
        }
      />
      <Heading.h2>You are the owner, you may set the price.</Heading.h2>
      <ContractForm
        contract="BootlegTraderApp"
        method="setTokenPrice"
        render={({
          inputs,
          inputTypes,
          state,
          handleInputChange,
          handleSubmit
        }) => (
          <Form onSubmit={handleSetTokenPriceSubmit(inputs, state)}>
            {inputs.map((input, index) => (
              <Input
                style={{ fontSize: 30 }}
                key={input.name}
                type={inputTypes[index]}
                name={input.name}
                value={state[input.name]}
                placeholder="Price in Eth"
                onChange={handleInputChange}
              />
            ))}
            <Button
              key="submit"
              type="button"
              onClick={handleSetTokenPriceSubmit(inputs, state)}
              size={'medium'}
            >
              List for sale
            </Button>
          </Form>
        )}
      />
    </>
  );
};

SellForm.contextTypes = { drizzle: PropTypes.object };

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(SellForm, mapStateToProps);
