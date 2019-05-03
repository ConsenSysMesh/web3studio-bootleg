import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData } from 'drizzle-react-components';
import { Card } from 'rimble-ui';

// Custom components
import TokenDetails from './TokenDetails';
import CurrentUserCard from './CurrentUserCard';
import BalanceCard from './BalanceCard';
import PurchaseForm from './PurchaseForm';
import SellForm from './SellForm';
import DownloadCard from './DownloadCard';

const TraderComponent = ({ accounts, drizzleStatus }, { drizzle }) => {
  return (
    <div className="App">
      <CurrentUserCard />
      <h1>Bootleg Token Trader</h1>
      <Card>
        <TokenDetails />
        <ContractData
          contract="BootlegTraderApp"
          method="getOwner"
          render={ownerAddress =>
            accounts[0] === ownerAddress ? <SellForm /> : <PurchaseForm />
          }
        />
      </Card>
      <ContractData
        contract="BootlegTraderApp"
        method="isTokenFranchisor"
        methodArgs={[accounts[0]]}
        render={isFranchisor =>
          isFranchisor ? (
            <>
              <DownloadCard />
              <BalanceCard />
            </>
          ) : (
            <h2>Video file available to franchisors only</h2>
          )
        }
      />
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
