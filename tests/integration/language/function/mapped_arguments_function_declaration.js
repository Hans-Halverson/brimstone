/*---
description: >
  A function declaration that shares a name with a function parameter shares a single binding with
  that parameter, so the mapping in a mapped arguments object must be preserved.
flags: [noStrict]
---*/

// The function declaration is hoisted and initialized before the body runs, so the mapped index
// holds the function itself instead of the supplied argument.
function initialValue(a) {
  function a() {}
  assert.sameValue(typeof a, "function");
  assert.sameValue(arguments[0], a);
}
initialValue(1);

// Writes through the binding name are visible through the arguments object
function writeName(a) {
  function a() {}
  a = 7;
  assert.sameValue(arguments[0], 7);
}
writeName(1);

// Writes through the arguments object are visible through the binding name
function writeArguments(a) {
  function a() {}
  arguments[0] = 9;
  assert.sameValue(a, 9);
}
writeArguments(1);

// Only the shadowed parameter is affected, other parameters remain mapped
function middleParameter(a, b, c) {
  function b() {}
  b = "B";
  assert.sameValue(arguments[1], "B");

  arguments[0] = "A";
  assert.sameValue(a, "A");
  arguments[2] = "C";
  assert.sameValue(c, "C");
}
middleParameter(1, 2, 3);

// Duplicate parameters share a binding referring to the last parameter, so only that index is
// mapped once a function declaration overwrites the binding.
function duplicateParameters(a, a) {
  function a() {}
  a = 7;
  assert.sameValue(arguments[0], 1);
  assert.sameValue(arguments[1], 7);
}
duplicateParameters(1, 2);

// Multiple function declarations with the same name still keep the mapping
function multipleDeclarations(a) {
  function a() {}
  function a() {}
  a = 7;
  assert.sameValue(arguments[0], 7);
}
multipleDeclarations(1);

// An Annex B block level function declaration also shares the parameter's binding
function blockLevelDeclaration(a) {
  {
    function a() {}
  }
  a = 7;
  assert.sameValue(arguments[0], 7);
}
blockLevelDeclaration(1);

// Parameters that were not supplied an argument are never mapped
function underapplied(a) {
  function a() {}
  a = 7;
  assert.sameValue(arguments.length, 0);
  assert.sameValue(arguments[0], undefined);
}
underapplied();
