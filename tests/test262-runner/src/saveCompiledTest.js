'use strict';

const fs = require('fs');
const path = require('path');

module.exports = function saveCompiledTest(test, options) {
  let outcome = test.result.pass ? 'pass' : 'fail';
  let scenario = test.scenario === 'strict mode' ? 'strict' : test.scenario;
  let savedTestPath = path.normalize(
    `${test.file}.${options.hostType}.${scenario}.${outcome}`
  );
  fs.writeFileSync(savedTestPath, test.compiled);
  return savedTestPath;
}
