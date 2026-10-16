/*---
description: >
  The name of a named function expression lives in its own scope enclosing the function scope.
flags: [noStrict]
---*/

// Eval var in the body shadows the function name
assert.sameValue((function nm() { eval("var nm = 1"); return typeof nm; })(), "number");
assert.sameValue((function nm() { eval("var nm = 1"); return nm; })(), 1);

// Both from inside the eval and from closures created after the eval
assert.sameValue((function nm() { return eval("var nm = 1; typeof nm"); })(), "number");
assert.sameValue((function nm() { eval("var nm = 1"); return (() => nm)(); })(), 1);
assert.sameValue((function nm() { eval("var nm = 1"); return function () { return nm; }; })()(), 1);

// Eval var in a parameter expression shadows the function name in the body
assert.sameValue((function nm(a = eval("var nm = 1")) { return typeof nm; })(), "number");

// A lexical declaration in the body of a function with parameter expressions does not conflict with
// the function name, since the name is not a parameter
assert.sameValue((function nm(a = 1) { let nm = 5; return nm; })(), 5);
assert.sameValue((function nm(a = nm) { let nm = 6; return typeof a; })(), "function");
