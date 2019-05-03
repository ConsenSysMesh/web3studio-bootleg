import React, { Component } from 'react';
import { DrizzleProvider } from 'drizzle-react';
import { LoadingContainer } from 'drizzle-react-components';
import { ThemeProvider } from 'rimble-ui';
import drizzleOptions from './drizzleOptions';
import TraderComponent from './TraderComponent';
import theme from './theme';

class App extends Component {
  render() {
    return (
      <DrizzleProvider options={drizzleOptions}>
        <ThemeProvider theme={theme}>
          <LoadingContainer>
            <TraderComponent />
          </LoadingContainer>
        </ThemeProvider>
      </DrizzleProvider>
    );
  }
}
export default App;
