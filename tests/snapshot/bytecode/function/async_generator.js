async function *empty() {}

async function *withArgsAndBody(a, b = 1, c = 2) {
  3;
}

async function *simpleYield() {
  1;
  yield;
  2;
}

async function *yieldWithArg() {
  1;
  yield 2;
  3;
}

async function *withReturn() {
  if (true) {
    return;
  } else {
    return 1;
  }
}

async function *withAwait() {
  1;
  await 2;
  3;
}

var global = 0;

async function *yieldDestination() {
  var x = 1;
  x = yield;
  global = yield;
}