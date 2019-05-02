import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import {
  AccountData,
  ContractData,
  ContractForm
} from 'drizzle-react-components';
import { Button, Form, Input, Card, ToastMessage } from 'rimble-ui';
import TokenCard from './TokenCard';
import CurrentUserCard from './CurrentUserCard';
import PropTypes from 'prop-types';

const TraderComponent = ({ accounts, drizzleStatus }, { drizzle }) => {
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
    <div className="App">
      <CurrentUserCard />
      <div className="section" style={{ padding: '1em' }}>
        <h1>The Bootleg Token Trader</h1>
        <TokenCard />
        <div style={{ clear: 'both', paddingTop: '1em' }}>
          <Card>
            <ContractData
              contract="BootlegTraderApp"
              method="tokenPrice"
              render={tokenPrice => (
                <>
                  <h2>
                    Current price is: {web3.utils.fromWei(tokenPrice, 'ether')}{' '}
                    Eth
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
              )}
            />
            <h2>List for sale</h2>
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
          </Card>
        </div>
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
                {inputs.map((input, index) => (
                  <Input
                    style={{ fontSize: 30 }}
                    key={input.name}
                    type={inputTypes[index]}
                    name={input.name}
                    value={state[input.name]}
                    placeholder={input.name}
                    onChange={handleInputChange}
                  />
                ))}
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
      </div>
    </div>
  );
};

TraderComponent.contextTypes = { drizzle: PropTypes.object };

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(TraderComponent, mapStateToProps);
