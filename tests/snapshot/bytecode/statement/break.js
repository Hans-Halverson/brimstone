function testSimple() {
  1;
  while (true) {
    2;
    break;
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
        break;
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
        break outer1;
      }
      if (4) {
        break outer2;
      }
      if (5) {
        break inner;
      }
    }
    5;
  }
  6;
}

function testLabeledNonLoop() {
  label: if (true) {
    1;
    break label;
    2;
  } else {
    3;
  }
  4;
}

function unlabeledBreakIgnoresLabeledStatement1() {
  while (true) {
    1;
    a: {
      2;
      break;
    }
    3;
  }
}

function unlabeledBreakIgnoresLabeledStatement2() {
  switch (true) {
    default: {
      1;
      a: {
        2;
        break;
      }
      3;
    }
  }
}
