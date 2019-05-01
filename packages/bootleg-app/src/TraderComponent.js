import React from 'react';
import { drizzleConnect } from 'drizzle-react';
import {
  AccountData,
  ContractData,
  ContractForm
} from 'drizzle-react-components';
import { Button, Form, Input, Card } from 'rimble-ui';

const tokenPurchasePrice = 1000000;

const TraderComponent = ({ accounts, drizzleStatus }) => (
  <div className="App">
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
    <div className="section" style={{ padding: '1em' }}>
      <h1>The Bootleg Token Trader</h1>
      <div
        style={{ display: 'block', overflowWrap: 'break-word', width: '850px' }}
      >
        <img
          style={{
            paddingRight: '1em',
            width: '350px',
            height: '200px',
            float: 'left'
          }}
          src="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg-cover.png"
        />
        <span style={{ fontSize: '2em' }}>Token #1</span>
        <br />
        <span style={{ fontSize: '2em' }}>outsideOUTSIDE SXSW2019 Show</span>
        <p>
          Recording of the artist outsideOUTSIDE at the Consensys SXSW
          Blockchain house. Captured March 13th, 2019 @ 8PM.{' '}
          <a href="https://www.outsideoutside.band">
            https://www.outsideoutside.band
          </a>
        </p>
        <p>
          Owned by:
          <ContractData contract="BootlegTraderApp" method="getOwner" />
        </p>
      </div>
      <div style={{ clear: 'both', paddingTop: '1em' }}>
        <h2>
          Current price is:{' '}
          <ContractData contract="BootlegTraderApp" method="tokenPrice" />
        </h2>
        <ContractForm
          contract="BootlegTraderApp"
          method="purchase"
          sendArgs={{ value: tokenPurchasePrice }}
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
                  value={state[input.name] || tokenPurchasePrice}
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
      <Card>
        <h2>
          Your Bootleg payment Balance:&nbsp;
          <ContractData contract="BootlegTraderApp" method="getBalance" />
        </h2>
        <p>
          This is the balance you have in payments owed to you from franchise
          royalties.
        </p>
      </Card>
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
