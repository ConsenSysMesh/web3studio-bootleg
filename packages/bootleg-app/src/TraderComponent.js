import React from 'react';
import PropTypes from 'prop-types';
import { drizzleConnect } from 'drizzle-react';
import { ContractData } from 'drizzle-react-components';
import { Box, PublicAddress, Card, Flex, Image } from 'rimble-ui';
import styled from 'styled-components';
import BootlegLogo from './BootlegLogo';
import TokenDetails from './TokenDetails';
import CurrentUserCard from './CurrentUserCard';
import BalanceCard from './BalanceCard';
import PurchaseForm from './PurchaseForm';
import SellForm from './SellForm';
import DownloadCard from './DownloadCard';

const Container = styled.div`
  max-width: 1000px;
  margin: 0 auto;
`;

const cardWidths = [1, 1, 1 / 2];

const TraderComponent = ({ accounts, drizzleStatus }, { drizzle }) => {
  return (
    <>
      <Box bg={'grey'} py={3}>
        <BootlegLogo width={'1em'} />
      </Box>

      <Container>
        <Image
          pt={4}
          borderRadius={8}
          alt="Bootleg Token Cover Art"
          src="https://s3.amazonaws.com/web3studio-bootlegs/outsideOUTSIDE-SXSW2019-Bootleg-cover.png"
        />
        <TokenDetails>
          <Box pt={4}>
            <Card>
              <ContractData
                contract="BootlegTraderApp"
                method="getOwner"
                render={ownerAddress =>
                  accounts[0] === ownerAddress ? <SellForm /> : <PurchaseForm />
                }
              />
            </Card>
          </Box>
        </TokenDetails>

        <Flex>
          <ContractData
            contract="BootlegTraderApp"
            method="isTokenFranchisor"
            methodArgs={[accounts[0]]}
            render={isFranchisor =>
              isFranchisor ? (
                <>
                  <DownloadCard mr={2} width={cardWidths} />
                  <BalanceCard ml={2} width={cardWidths} />
                </>
              ) : null
            }
          />
        </Flex>
        <Box width={cardWidths}>
          <ContractData
            contract="BootlegTraderApp"
            method="getOwner"
            render={ownerAddress => (
              <Box pt={4}>
                <PublicAddress label={'Current Owner'} address={ownerAddress} />
              </Box>
            )}
          />
        </Box>
        <CurrentUserCard />
      </Container>
    </>
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
