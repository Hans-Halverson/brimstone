function testSimple() {
  1;
  while (true) {
    2;
    continue;
    // Not reached due to abrupt completion
    3;
  }
}

function testNestedUnlabeled() {
  1;
  outer: while (true) {
    2;
    while (false) {
      if (3) {
        continue;
      }
    }
    4;
  }
  5;
}

function testLabeled() {
  1;
  outer1: outer2: while (true) {
    2;
    inner: while (false) {
      if (3) {
        continue outer1;
      }
      if (4) {
        continue outer2;
      }
      if (5) {
        continue inner;
      }
    }
    6;
  }
  7;
}

function continueSkipsLabeledStatement() {
  while (true) {
    1;
    a: {
      2;
      continue;
    }
    3;
  }
}

function continueSkipsSwitch() {
  while (true) {
    1;
    switch (false) {
      default:
        2;
        continue;
    }
    3;
  }
}
