function *empty() {}

function *withArgsAndBody(a, b = 1, c = 2) {
  3;
}

function *simpleYield() {
  1;
  yield;
  2;
}

function *yieldWithArg() {
  1;
  yield 2;
  3;
}

function *multipleYields() {
  yield;
  yield 1;
  yield;
  yield 2;
}

function *yieldReturnValue() {
  return (yield) + (yield 1);
}

var global = 1;

function *yieldDestination() {
  var a = 1;

  // Completion value placed in temporary before completion type test
  a = yield;

  global = yield;
  return yield;
}

function *yieldInFinally() {
  try {
    yield;
  } finally {
    1;
  }
}

(function *generatorExpression() {});

// Anonymous generator expression
(function *() {});

({
  *generatorMethod() {},
});

class C {
  *generatorMethod() {}
}