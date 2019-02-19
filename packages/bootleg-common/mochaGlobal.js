const expect = require('jest-matchers');

global.expect = expect;

after(async () => {
  // `coverageSubProvider` defined in ./truffle-config
  await global.coverageSubProvider.writeCoverageAsync();
});
