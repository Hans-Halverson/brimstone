const yargs = require('yargs');
const yargv = yargs
  .strict()
  .usage('Usage: brimstone-test262-runner [options] <test-file-glob>')
  .describe('host-path', 'path to host binary')
  .describe('host-args', 'command-line arguments to pass to host')
  .describe('test262-dir', 'test262 root directory')
  .describe('includes-dir', 'directory where helper files are found')
  .describe('threads', 'number of threads to use')
  .describe('prelude', 'content to include above each test; supports multiple --prelude parameters')
  .describe('version', 'print version of test262-harness')
  .alias('version', 'v')
  .describe('features', 'comma-separated list of features to filter for')
  .nargs('features', 1)
  .alias('features', 'f')
  .describe('features-exclude', 'comma-separated list of features to filter for exclusion')
  .nargs('features-exclude', 1)
  .alias('features-exclude', 'fe')
  .describe('features-include', 'comma-separated list of features to filter for inclusion')
  .nargs('features-include', 1)
  .alias('features-include', 'fi')
  .describe('transformer', 'path to module which exports a code transformer function')
  .describe('preprocessor', 'path to module which exports a map function that operates on each Test262Test object before it executed')
  .nargs('prelude', 1)
  .nargs('threads', 1)
  .default('threads', 1)
  .alias('threads', 't')
  .describe('reporter', 'format of data written to standard output')
  .choices('reporter', ['simple', 'json'])
  .nargs('reporter', 1)
  .alias('reporter', 'r')
  .default('reporter', 'simple')
  .describe('reporter-keys', 'comma-separated list of keys to include in JSON ouput')
  .help('help')
  .alias('help', 'h')
  .describe('timeout', 'test timeout (in ms, default 10000)')
  .nargs('timeout', 1)
  .describe('accept-version', 'override for supported Test262 version')
  .boolean('save-compiled-tests')
  .describe('save-compiled-tests', 'Write the compiled version of path/to/test.js as path/to/test.js.<host-type>.<default|strict>.<pass|fail> so that it can be easily re-run under that host')
  .boolean('save-only-failed')
  .describe('save-only-failed', 'Only save the compiled version of the test if it failed, to help easily repro failed tests (implies --saveCompiledTests)')
  .describe('error-for-failures', 'Return a non-zero exit code if one or more tests fail')
  .example('brimstone-test262-runner path/to/test.js')
  .example('brimstone-test262-runner --host-path path/to/host path/to/test262/test/folder/**/*.js')
  .example('brimstone-test262-runner --host-path path/to/host --save-compiled-tests path/to/test262/test/folder/**/*.js')
  .example('brimstone-test262-runner --host-path path/to/host --save-only-failed path/to/test262/test/folder/**/*.js')
  .fail((msg, err) => {
    if (err) {
      console.error(err.stack);
    } else {
      console.error(msg);
    }
    process.exit(1);
  });

exports.argv = yargv.argv;
exports.showHelp = () => yargv.showHelp();
