import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import { ContractData, ContractForm } from 'drizzle-react-components';
import { Button, Form, Input } from 'rimble-ui';

const TraderComponent = ({ accounts, drizzleStatus }) => (
  <div className="App">
    <div>
      <h1>Drizzle Examples</h1>
      <p>Examples of how to get started with Drizzle in various situations.</p>
    </div>
    <div className="section">
      <h2>The Token</h2>
      <ContractData contract="BootlegTraderApp" method="bootlegTokenId" />
      <h2>Token URI</h2>
      <ContractData contract="BootlegTraderApp" method="getTokenURI" />
      <h2>Token Owner</h2>
      <ContractData contract="BootlegTraderApp" method="getOwner" />
      <h2>Your Balance</h2>
      <ContractData contract="BootlegTraderApp" method="getBalance" />
      <h2>Active Account</h2>
      {JSON.stringify(accounts)}
      <h2>Purchase</h2>
      <ContractForm
        contract="BootlegTraderApp"
        method="purchase"
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
              List for sale
            </Button>
          </Form>
        )}
      />
    </div>
  </div>
);

const mapStateToProps = state => {
  return {
    accounts: state.accounts,
    drizzleStatus: state.drizzleStatus
  };
};

export default drizzleConnect(TraderComponent, mapStateToProps);
