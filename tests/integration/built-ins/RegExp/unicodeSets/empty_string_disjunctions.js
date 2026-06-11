/*---
description: Character class string disjunctions may contain the empty string.
---*/

// Simple tests for empty string matches
assert.compareArray(/[\q{}]/v.exec(''), ['']);
assert(/[\q{}]/v.test('abc'));
assert.sameValue(/[\q{}]/v.exec('x').index, 0);
assert.compareArray(/x[\q{}]y/v.exec('xy'), ['xy']);

// Empty alternative may appear alongside non-empty alternatives, still matches
assert.compareArray(/[\q{|a}]/v.exec('x'), ['']);
assert.compareArray(/[\q{ab|}]/v.exec('x'), ['']);
assert.compareArray(/[\q{||}]/v.exec('x'), ['']);

// Empty string unioned with a code point, code point matches first
assert.compareArray(/[a\q{}]/v.exec(''), ['']);
assert.compareArray(/[a\q{}]/v.exec('a'), ['a']);

// Empty string unioned with a string, string matches first
assert.compareArray(/[\q{abc}\q{}]/v.exec(''), ['']);
assert.compareArray(/[\q{abc}\q{}]/v.exec('abc'), ['abc']);

// Empty string matches at every index unless code point matches first
const matches = Array.from('babaa'.matchAll(/[\q{a|}]/gv)).map(m => [m[0], m.index]);
assert.compareArray(matches[0], ['', 0]);
assert.compareArray(matches[1], ['a', 1]);
assert.compareArray(matches[2], ['', 2]);
assert.compareArray(matches[3], ['a', 3]);
assert.compareArray(matches[4], ['a', 4]);
assert.compareArray(matches[5], ['', 5]);

// Set operations that resolve to the empty string
assert.compareArray(/x[\q{}&&\q{|x}]y/v.exec('xy'), ['xy']);
assert.compareArray(/x[\q{a|}--\q{a}]y/v.exec('xy'), ['xy']);

// Empty string inside of a quantifier
assert.compareArray(/[\q{}]*/v.exec('ab'), ['']);
assert.compareArray(/[\q{a|}]*/v.exec('aa'), ['aa']);
assert.compareArray(/[\q{a|}]+/v.exec('b'), ['']);
assert.compareArray(/[\q{ab|}]*/v.exec('abab'), ['abab']);