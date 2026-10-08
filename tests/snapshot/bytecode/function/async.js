async function empty() {}

async function returns() {
  if (true) {
    return 1;
  }

  return 2;
}

async function awaits() {
  1;
  await (2 + 3);
  4;
}

async function paramExpressions(x = 1 + 2) {
  3;
}

async function returnInFinally() {
  try {
    1;
    return 2;
  } finally {
    3;
  }
}

var global = 1;

async function awaitDestination() {
  var a = 1;

  // Completion value placed in temporary before completion type test
  a = await 2;

  global = await 3;
  -(await 4);
  return await 5;
}