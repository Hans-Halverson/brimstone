'use strict';

const fs = require('fs');
const Path = require('path');

module.exports = function findTest262Root(path) {
  if (/\*/.test(path)) {
    return findTest262Root(Path.resolve(path, '..'));
  }
  const stat = fs.statSync(path);
  if (stat.isFile()) {
    path = Path.dirname(path);
  }

  const contents = fs.readdirSync(path)
  if (contents.indexOf('README.md') > -1
      && contents.indexOf('package.json') > -1
      && contents.indexOf('test') > -1
      && contents.indexOf('harness') > -1) {
    return path;
  }

  const parent = Path.resolve(path, '../');

  if (parent === path) {
    return null;
  }

  return findTest262Root(parent);
};
