/*---
description: ^
  Set operations in case insensitive mode treat case equivalent strings as the same string.
---*/

function matches(regexp, str) {
  const result = regexp.exec(str);
  assert(result !== null);
  assert.sameValue(result[0], str);
}

function notMatches(regexp, str) {
  const result = regexp.exec(str);
  assert(result === null || result[0] !== str);
}

// Intersection operates on case folded strings
{
  const regexp = /[\q{AB}&&\q{ab}]/iv;
  matches(regexp, "ab");
  matches(regexp, "AB");
  matches(regexp, "aB");
  matches(regexp, "Ab");
}

// Difference operates on case folded strings
{
  const regexp = /[\q{ab|cd}--\q{AB}]/iv;
  notMatches(regexp);
  notMatches(regexp, "AB");
  matches(regexp, "cd");
  matches(regexp, "CD");
}

// Nested set operations operate on case folded strings
{
  const regexp = /[[[\q{AB}\q{cd}\q{ef}]&&[\q{ab}\q{CD}]]--\q{cD}]/iv;
  matches(regexp, "ab");
  matches(regexp, "AB");
  notMatches(regexp, "cd");
  notMatches(regexp, "CD");
  notMatches(regexp, "ef");
}

// Strings are not case folded without the `i` flag
{
  const regexp = /[\q{AB}&&\q{ab}]/v;
  notMatches(regexp, "ab");
  notMatches(regexp, "AB");
}
