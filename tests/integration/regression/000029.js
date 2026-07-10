/*---
description: >
  A lazy quantifier inside a greedy quantifier could trigger a progress check bug that resulted in
  incorrect matches.
---*/

assert.compareArray(/(a?(b)*?)*/.exec('ab'), ['ab', 'b', 'b']);
assert.compareArray(/(a?b*?)*/.exec('ab'), ['ab', 'b']);
assert.compareArray(/(a{0,2}(\w*)*?)*/.exec('ab'), ['ab', 'b', 'b']);
assert.compareArray(/(b{0,2}([a-c]*?)*?){2,}/.exec('aabbcc'), ['aabbcc', 'c', 'c']);