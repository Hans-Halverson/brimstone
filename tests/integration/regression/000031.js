/*---
description: ^
  Global var and function declarations must be created (and therefore enumerated) in declaration
  order. Previously the names were collected into a HashSet before being installed on the global
  object, producing a nondeterministic order that varied between runs and separated functions from
  vars instead of interleaving them in declaration order.
includes: [compareArray.js]
---*/

var g_zebra = 1;
function g_apple() {}
var g_mango = 2;
function g_delta() {}
// Redeclaring g_apple as a var must not move it from its first-declaration position.
var g_apple = 3;
var g_yak = 4;

assert.compareArray(
  Object.keys(globalThis).filter((k) => k.startsWith("g_")),
  ["g_zebra", "g_apple", "g_mango", "g_delta", "g_yak"]
);

// The bindings are enumerable own data properties regardless of kind.
assert.compareArray(
  Object.getOwnPropertyNames(globalThis).filter((k) => k.startsWith("g_")),
  ["g_zebra", "g_apple", "g_mango", "g_delta", "g_yak"]
);
