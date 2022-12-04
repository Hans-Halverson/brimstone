#!/usr/bin/env node

// Copyright (C) 2014, Microsoft Corporation. All rights reserved.
// This code is governed by the BSD License found in the LICENSE file.
const DEFAULT_TEST_TIMEOUT = 10000;

const fs = require('fs');
const path = require('path');
const util = require('util');

const { zip } = require('rxjs');
const { flatMap, filter, map } = require('rxjs/operators');

const AgentPool = require('./agent-pool.js');
const TestStream = require('./test-stream');
const ResultsEmitter = require('./results-emitter.js');
const cli = require('./cli.js');
const test262Finder = require('./findTest262.js');
const validator = require('./validator.js');

const argv = cli.argv;

// test262 directory (used to locate includes unless overridden with includesDir)
let test262Dir = argv.test262Dir;
// where to load includes from (usually a subdirectory of test262dir)
let includesDir = argv.includesDir;

let tempDir = argv.tempDir;
let acceptVersion = argv.acceptVersion;

// print version of test262-harness
if (argv.version) {
  printVersion();
  return;
}

// initialize reporter by attempting to load lib/reporters/${reporter}
// defaults to 'simple'
let reporter;
let reporterOpts = {};
if (fs.existsSync(path.join(__dirname, './reporters', `${argv.reporter}.js`))) {
  reporter = require(`./reporters/${argv.reporter}.js`);
} else {
  console.error(`Reporter ${argv.reporter} not found.`);
  process.exitCode = 1;
  return;
}

if (argv.reporterKeys) {
  reporterOpts.reporterKeys = argv.reporterKeys.split(',');
}

// Using argv.saveOnlyFailed implies argv.saveCompiledTests
if (argv.saveOnlyFailed && !argv.saveCompiledTests) {
  argv.saveCompiledTests = true;
}

if (argv.saveCompiledTests) {
  reporterOpts.saveCompiledTests = argv.saveCompiledTests;
  if (argv.saveOnlyFailed) {
    reporterOpts.saveOnlyFailed = argv.saveOnlyFailed;
  }
}

// load preload contents
let preludeContents = '';
if (argv.prelude) {
  if (!Array.isArray(argv.prelude)) {
    argv.prelude = [argv.prelude];
  }
  preludeContents = argv.prelude.map(prelude => fs.readFileSync(prelude, 'utf8')).join('\n');
}

// Select hostType and hostPath. hostType defaults to 'node'.
// If using default hostType, hostPath defaults to the current node executable location.
let hostType;
let hostPath;
let featuresInclude;
let featuresExclude;

if (argv.hostType) {
  hostType = argv.hostType;

  if (!argv.hostPath) {
    console.error('Missing host path. Pass --hostPath with a path to the host executable you want to test.');
    process.exitCode = 1;
    return;
  }

  hostPath = argv.hostPath;
} else {
  hostType = 'node';

  if (argv.hostPath) {
    hostPath = argv.hostPath;
  } else {
    hostPath = process.execPath;
  }
}

if (hostType) {
  reporterOpts.hostType = hostType;
}

let timeout = argv.timeout || DEFAULT_TEST_TIMEOUT;
let transform, transformer;
let preprocessor;

if (argv.transformer) {
  const transformerPath = path.isAbsolute(argv.transformer) ?
    argv.transformer : path.join(process.cwd(), argv.transformer)

  transform = transformer = require(transformerPath);
}

if (argv.preprocessor) {
  const preprocessorPath = path.isAbsolute(argv.preprocessor) ?
    argv.preprocessor : path.join(process.cwd(), argv.preprocessor)

  preprocessor = require(preprocessorPath);
}

if (argv.features || argv.featuresInclude) {
  featuresInclude = (argv.features || argv.featuresInclude).split(',').map(feature => feature.trim());
}

if (argv.featuresExclude) {
  featuresExclude = argv.featuresExclude.split(',').map(feature => feature.trim());
}

// Show help if no arguments provided
if (!argv._.length) {
  cli.showHelp();
  process.exitCode = 1;
  return;
}

// Test Pipeline
const pool = new AgentPool(
  Number(argv.threads), hostType, argv.hostArgs, hostPath, { tempDir, timeout, transform }
);

if (!test262Dir) {
  test262Dir = test262Finder(argv._[0]);
}
reporterOpts.test262Dir = test262Dir;

const remove = path.relative(process.cwd(), test262Dir);

argv._ = argv._.map(p => path.relative(remove, p));

let test262Version;
try {
  test262Version = JSON.parse(
    fs.readFileSync(path.join(test262Dir, 'package.json'))
  ).version;
} catch (err) {
  console.error(`Unable to detect version of test262: ${err}`);
  process.exitCode = 1;
  return;
}

if (!acceptVersion) {
  acceptVersion = test262Version;
}

const stream = new TestStream(test262Dir, includesDir, acceptVersion, argv._);

let tests = stream.pipe(filter(filterByFeatureInclude)).pipe(filter(filterByFeatureExclude)).pipe(map(insertPrelude));

if (preprocessor) {
  tests = tests.pipe(filter(preprocessor));
}

const results = zip(pool, tests).pipe(
  flatMap(pair => {
    return pool.runTest(pair);
  })
).pipe(
  map(test => {
    test.result = validator(test);
    return test;
  })
);

const emitter = new ResultsEmitter(results);

if (argv.errorForFailures) {
  emitter.on('fail', function () {
    process.exitCode = 1;
  });
}

reporter(emitter, reporterOpts);

function printVersion() {
  const p = require(path.resolve(__dirname, '..', 'package.json'));
  console.log(`v${p.version}`);
}

function insertPrelude(test) {
  const index = test.insertionIndex;
  if (index === -1) {
    return test;
  }

  if (preludeContents) {
    test.contents = test.contents.slice(0, index) +
      preludeContents +
      test.contents.slice(index);
  }

  return test;
}

function filterByFeatureInclude(test) {
  if (!featuresInclude) {
    return true;
  }
  return featuresInclude.some(feature => (test.attrs.features || []).includes(feature));
}

function filterByFeatureExclude(test) {
  if (!featuresExclude) {
    return true;
  }
  return !featuresExclude.some(feature => (test.attrs.features || []).includes(feature));
}