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