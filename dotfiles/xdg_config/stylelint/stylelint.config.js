const path = require('path')
module.exports = {
  extends: path.join(
    process.env.NPM_CONFIG_PREFIX,
    'lib', 'node_modules', 'stylelint-config-standard'
  ),
}