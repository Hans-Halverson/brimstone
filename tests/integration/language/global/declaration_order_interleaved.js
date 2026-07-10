/*---
description: >
  Interleaved global var and function declarations preserve declaration order, and redeclarations
  do not change the order of the original declaration.
includes: [compareArray.js]
---*/

var g_one = 1;
function g_two() {}
var g_three = 2;
function g_four() {}

// Redeclaring g_two as a var must not move it from its original order in the global object.
var g_two = 3;

var g_five = 4;

assert.compareArray(
  Object.keys(globalThis).filter((k) => k.startsWith("g_")),
  ["g_one", "g_two", "g_three", "g_four", "g_five"]
);

// The bindings are enumerable own data properties regardless of kind.
assert.compareArray(
  Object.getOwnPropertyNames(globalThis).filter((k) => k.startsWith("g_")),
  ["g_one", "g_two", "g_three", "g_four", "g_five"]
);
