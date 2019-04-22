import React, { Component } from 'react';
import { DrizzleProvider } from 'drizzle-react';
import { LoadingContainer } from 'drizzle-react-components';
import drizzleOptions from './drizzleOptions';
import TraderComponent from './TraderComponent';

class App extends Component {
  render() {
    return (
      <DrizzleProvider options={drizzleOptions}>
        <LoadingContainer>
          <TraderComponent />
        </LoadingContainer>
      </DrizzleProvider>
    );
  }
}
export default App;
