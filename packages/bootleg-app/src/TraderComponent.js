import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData, ContractForm } from 'drizzle-react-components';
import { Button, Form, Input, Card } from 'rimble-ui';

// Custom components
import TokenDetails from './TokenDetails';
import CurrentUserCard from './CurrentUserCard';
import BalanceCard from './BalanceCard';
import PurchaseForm from './PurchaseForm';
import SellForm from './SellForm';
import DownloadCard from './DownloadCard';

const TraderComponent = ({ accounts, drizzleStatus }, { drizzle }) => {
  const { web3, contracts } = drizzle;

  const isOwner = () => {
    return accounts[0] == contracts.BootlegTraderApp.methods.getOwner();
  };

  const isFrancisor = () => {
    return true;
  };

  return (
    <div className="App">
      <CurrentUserCard />
      <h1>The Bootleg Token Trader</h1>
      <Card>
        <TokenDetails />
        {!isOwner() && <PurchaseForm />}
        {isOwner() && <SellForm />}
      </Card>
      {isFrancisor() && <DownloadCard />}
      <BalanceCard />
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
