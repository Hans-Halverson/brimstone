function *foo() {
  yield;

  yield

  { yield }

  (yield);

  [yield];

  yield 1;

  yield* 1;
}

function *yield() {}