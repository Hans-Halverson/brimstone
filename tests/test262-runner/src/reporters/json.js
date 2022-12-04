'use strict';
const pick = require('../pick');
const saveCompiledTest = require('../saveCompiledTest');

function jsonReporter(results, opts) {
  let started = false;

  results.on('start', function () {
    console.log('[');
  });

  results.on('end', function () {
    console.log(']');
  });

  results.on('pass', function (test) {
    if (opts.saveCompiledTests && !opts.saveOnlyFailed) {
      // don't log here but include the path in the JSON output
      test.savedTestPath = saveCompiledTest(test, opts);
    }
  });

  results.on('fail', function (test) {
    if (opts.saveCompiledTests) {
      // don't log here but include the path in the JSON output
      test.savedTestPath = saveCompiledTest(test, opts);
    }
  });

  results.on('test end', function (test) {
    let toEmit;

    if (started) {
      process.stdout.write(',');
    } else {
      started = true;
    }

    if (opts.reporterKeys) {
      toEmit = pick(test, opts.reporterKeys);
    } else {
      toEmit = test;
    }

    console.log(JSON.stringify(toEmit));
  });
}

module.exports = jsonReporter;
