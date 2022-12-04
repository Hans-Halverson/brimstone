'use strict';

const path = require('path');

const minimatch = require('minimatch');
const {Subject} = require('rxjs');
const Test262Stream = require('test262-stream');

// test262-harness accepts file names and patterns as input; test262-stream
// accepts file names and directory names. To accomodate this, file patterns
// containing globs are truncated to the most precise directory name. All files
// within this directory are visited by test262-stream, but only those tests
// which match the pattern are emitted to from this module.
function patternsToDirectories(patterns) {
  return patterns.map((pattern) => {
    const parts = [];

    pattern.split(path.sep).every((part) => {
      if (part.indexOf('*') > -1 || part.slice(-3).toLowerCase() === '.js') {
        return false;
      }

      parts.push(part);

      return true;
    });

    return `${parts.join(path.sep)}${path.sep}`;
  })
  .filter((name) => name !== '')
  // Remove elements that would cause the same files to be visited more than
  // once:
  //
  // - duplicate elements (e.g. `a/b` if `a/b` is present elsewhere)
  // - elements which describe subdirectories of of some other element (e.g.
  //   `c/d/e` if `c/d` is also present)
  .filter((name, index, all) => {
    return all.every((other, otherIndex) => {
      // Allow only the first occurrence of repeated values (de-duplicate)
      if (name === other) {
          return index <= otherIndex;
      }

      // When one value contains another, only allow the shorter of the two
      const min = Math.min(name.length, other.length);
      if (name.substr(0, min) === other.substr(0, min)) {
        return name.length < other.length;
      }

      // All other values are unique and should be allowed
      return true;
    });
  }).map(dir => dir.slice(0, -1));
}

class TestStream extends Subject {
  constructor(test262Dir, includesDir, acceptVersion, globPatterns) {
    super();
    const paths = patternsToDirectories(globPatterns);
    const stream = new Test262Stream(test262Dir, {
      includesDir, paths, acceptVersion
    });

    stream.on('data', (test) => {
      if (!globPatterns.some((pattern) => minimatch(test.file, pattern))) {
        return;
      }

      // Preserve the relative file path from Test262, ignore the initial test/
      // folder.
      test.relative = path.relative('test', test.file);

      // The file paths emitted by test262-stream are relative to the provided
      // Test262 directory. To ensure backwards compatability, they must be
      // modified to be in terms of the current working directory.
      test.file = path.relative(process.cwd(), path.join(test262Dir, test.file));

      this.next(test);
    });
    stream.on('error', (error) => this.error(error));
    stream.on('end', () => {
      this.complete();
    });
  }
}

module.exports = TestStream;
