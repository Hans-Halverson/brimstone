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