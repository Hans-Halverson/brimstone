'use strict';
const path = require('path');
const saveCompiledTest = require('../saveCompiledTest');

function simpleReporter(results, opts) {
  let passed = 0;
  let failed = 0;
  let lastPassed = true;

  results.on('pass', function (test) {
    passed++;

    clearPassed();
    lastPassed = true;
    process.stdout.write(`PASS ${test.file}`);

    if (opts.saveCompiledTests && !opts.saveOnlyFailed) {
      test.savedTestPath = saveCompiledTest(test, opts);
      process.stdout.write(`\nSaved compiled passed test as ${test.savedTestPath}\n`);
    }
  });

  results.on('fail', function (test) {
    failed++;
    clearPassed();
    lastPassed = false;
    console.log(`FAIL ${test.file} (${test.scenario})`);
    console.log(`  ${test.result.message}`);
    console.log('');

    if (opts.saveCompiledTests) {
      test.savedTestPath = saveCompiledTest(test, opts);
      process.stdout.write(`Saved compiled failed test as ${test.savedTestPath}\n`);
    }
  });

  results.on('end', function () {
    clearPassed();

    console.log(`Ran ${(passed + failed)} tests`);
    console.log(`${passed} passed`);
    console.log(`${failed} failed`);
  });

  function clearPassed() {
    if (lastPassed) {
      if (process.stdout.isTTY) {
        process.stdout.clearLine();
        process.stdout.cursorTo(0);
      } else {
        process.stdout.write('\n');
      }
    }
  }
}

module.exports = simpleReporter;
