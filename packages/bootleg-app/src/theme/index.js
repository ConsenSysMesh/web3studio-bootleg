import { theme } from 'rimble-ui';
import { red, orange, yellow, green, blue, purple, grey } from './colors';

const breakpointEms = [40, 52, 64, 80, 125];

export default {
  ...theme,
  fonts: {
    serif: 'athelas, georgia, times, serif',
    sansSerif: '"Source Sans Pro", -apple-system, sans-serif',
    monospace: '"Source Code Pro", monospace'
  },
  fontSizes: [14, 16, 20, 24, 30, 36, 48, 70],
  breakpointEms,
  breakpoints: breakpointEms.map(n => n + 'em'),
  colors: {
    primary: purple[400],
    copyColor: grey[800],
    red: red[500],
    orange: orange[500],
    yellow: yellow[400],
    green: green[500],
    blue: blue[500],
    grey: grey[100],
    purple: purple[500],
    black: grey[900],
    white: '#ffffff',
    transparent: 'transparent',
    blacks: [
      'rgba(0,0,0,.0125)',
      'rgba(0,0,0,.025)',
      'rgba(0,0,0,.05)',
      'rgba(0,0,0,.1)',
      'rgba(0,0,0,.2)',
      'rgba(0,0,0,.3)',
      'rgba(0,0,0,.4)',
      'rgba(0,0,0,.5)',
      'rgba(0,0,0,.6)',
      'rgba(0,0,0,.7)',
      'rgba(0,0,0,.8)',
      'rgba(0,0,0,.9)'
    ],
    palette: { red, orange, yellow, green, blue, purple, grey, primary: purple }
  },
  shadows: [
    '0',
    '0px 2px 4px rgba(0, 0, 0, 0.1)',
    '0px 2px 4px rgba(0, 0, 0, 0.2)',
    '0px 2px 6px rgba(0, 0, 0, 0.3)',
    '0px 8px 16px rgba(0, 0, 0, 0.1)'
  ]
};
