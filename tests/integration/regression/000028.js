/*---
description: >
  A repetition within a lookbehind was failing to clear captures correctly, causing a backreference
  to incorrectly match a capture which is not complete.
---*/

// Lookbehind cases that were incorrectly capturing
assert.sameValue(/(?<=(a(a\2*)?){2})/.test("aa"), true);
assert.sameValue(/(?<=(a(a\2*)?){2})/.test("aaa"), true);
assert.compareArray(/(?<=(a(a\2*)?){2})/.exec("aa"), ["", "a", undefined]);
assert.compareArray(/(?<=(a(a\2*)?){3})/.exec("aaa"), ["", "a", undefined]);
assert.compareArray(/(?<=((a(a\3*)?){2}))/.exec("aa"), ["", "aa", "a", undefined]);

// Mixing lookahead and lookbehind
assert.compareArray(/(?<=((?=(a\2*))a){2})/.exec("aa"), ["", "a", "a"]);

// Lookbehind within a forwards repetition
assert.compareArray(/((?<=(a\2*)a)){2}/.exec("aa"), ["", "", "a"]);