/*---
description: >
  When a function has parameter expressions its body is a separate var scope. A body `var` that
  redeclares a parameter (or `arguments`) must be initialized from the parameter's value.
flags: [noStrict]
---*/

// A body var with the same name as a parameter starts with the parameter's value
function simple(x = 1) {
  var x;
  return x;
}
assert.sameValue(simple(), 1);
assert.sameValue(simple(7), 7);

// The copy happens at body entry, not at the var declaration
function useBeforeDeclaration(a = 1) {
  var before = a;
  var a;
  return before;
}
assert.sameValue(useBeforeDeclaration(4), 4);

// Closures in the parameter list see the parameter, closures in the body see the body var
function closures(x = 1, paramClosure = () => x) {
  var x = x + 10;
  var bodyClosure = () => x;
  assert.sameValue(x, 11);
  assert.sameValue(paramClosure(), 1);
  assert.sameValue(bodyClosure(), 11);
}
closures();

// A parameter captured before it is initialized is still copied correctly
function capturedBeforeInit(g = () => b, b = 1) {
  var b;
  return b;
}
assert.sameValue(capturedBeforeInit(), 1);

// A body function declaration overwrites the parameter instead of being copied over
function functionDeclaration(a = 0) {
  function a() {}
  return typeof a;
}
assert.sameValue(functionDeclaration(), "function");

// A body `var arguments` is initialized from the arguments object, which must be created even
// though no use of `arguments` resolves to the function scope.
function argumentsVar(x = 0) {
  var arguments;
  return arguments.length;
}
assert.sameValue(argumentsVar(1, 2, 3), 3);

// A parameter named `arguments` is copied like any other parameter
function argumentsParameter(arguments = 5) {
  var arguments;
  return arguments;
}
assert.sameValue(argumentsParameter(), 5);
assert.sameValue(argumentsParameter(9), 9);

// An `arguments` reference in the parameter list and the body var refer to the same object
function argumentsInParameters(a = arguments) {
  var arguments;
  return arguments === a;
}
assert.sameValue(argumentsInParameters(), true);

// A function expression named `arguments` keeps its name outside the function scope, so a body
// `var arguments` is still initialized from the implicit arguments object.
var argumentsNamedExpression = function arguments(x = 0) {
  var arguments;
  return arguments.length;
};
assert.sameValue(argumentsNamedExpression(1, 2), 2);

// Arrow functions have no arguments object so the body var stays undefined
var argumentsArrow = (x = 0) => {
  var arguments;
  return typeof arguments;
};
assert.sameValue(argumentsArrow(), "undefined");

// A direct eval in the body declares vars in the body var scope. With no static body var the eval
// creates a fresh binding (initialized to undefined) that shadows the parameter.
function evalVar(a = 1) {
  eval("var a");
  return a;
}
assert.sameValue(evalVar(), undefined);

// With a static body var the eval operates on the already-copied binding
function evalVarWithBodyVar(a = 1) {
  var a;
  eval("var a");
  return a;
}
assert.sameValue(evalVarWithBodyVar(), 1);

function evalBeforeDeclaration(a = 1) {
  eval("var a");
  var a;
  return a;
}
assert.sameValue(evalBeforeDeclaration(), 1);

// Only a static `var arguments` creates the arguments object, not one declared by eval
function evalDeclaresArguments(x = 0) {
  eval("var arguments");
  return typeof arguments;
}
assert.sameValue(evalDeclaresArguments(), "undefined");
