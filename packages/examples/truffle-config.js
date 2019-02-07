if (__dirname.includes('coverageEnv')) {
  module.exports = require('../../../truffle-config');
} else {
  module.exports = require('../../truffle-config');
}
