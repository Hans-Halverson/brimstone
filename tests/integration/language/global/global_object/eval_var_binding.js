/*---
description: >
  Var and function bindings created by a direct eval in global scope reuse the existing global
  property, keeping caches valid.
flags: [noStrict]
---*/

// Warm a load callsite in a separate script so it uses a global property cache.
Object.defineProperty(globalThis, "a", { value: 1, writable: true, configurable: true });
$262.evalScript("globalThis.getA = function () { return a; };");
assert.sameValue(getA(), 1);
assert.sameValue(getA(), 1);

// A redeclaring var in a direct eval keeps the existing property and stores through it.
eval("var a = 9;");
assert.sameValue(getA(), 9);
assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "a").value, 9);
assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "a").configurable, true);

// A new var in a direct eval creates a deletable global property.
eval("var b = 4;");
assert.sameValue(globalThis.b, 4);
assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "b").configurable, true);
assert.sameValue(delete globalThis.b, true);

// A var in a direct eval does not overwrite an existing value.
Object.defineProperty(globalThis, "c", { value: 1, writable: true, configurable: true });
$262.evalScript("globalThis.getC = function () { return c; };");
assert.sameValue(getC(), 1);
eval("var c;");
assert.sameValue(getC(), 1);

// A function declaration in a direct eval replaces the value of a warm cached property.
Object.defineProperty(globalThis, "d", { value: 1, writable: true, configurable: true });
$262.evalScript("globalThis.getD = function () { return d; };");
assert.sameValue(getD(), 1);
assert.sameValue(getD(), 1);
eval("function d() { return 2; }");
assert.sameValue(typeof getD(), "function");
assert.sameValue(getD()(), 2);

// A var in a direct eval at global scope cannot shadow an existing global lexical binding.
// The eval must stay at global scope, otherwise the var lands in the enclosing function scope.
$262.evalScript("let e = 1;");
var evalError = null;
try {
  eval("var e = 2;");
} catch (err) {
  evalError = err;
}
assert.sameValue(evalError instanceof SyntaxError, true);

// A global lexical binding cannot be redeclared over a non-configurable global var.
assert.throws(SyntaxError, function () { $262.evalScript("let nonConfigurable = 1;"); });

var nonConfigurable = 1;
