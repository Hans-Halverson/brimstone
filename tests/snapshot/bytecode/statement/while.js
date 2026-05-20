function testBasic() {
  1;
  while (2) {
    3;
  }
  4;
}

function testBooleanTest() {
  1;
  while (true) {
    2;
  }
  3;
}

function testAbruptBody() {
  1;
  while (true) {
    2;
    return;
    3;
  }
  4;
}

function testBreakAndContinue() {
  1;
  while (true) {
    if (2) {
      break;
    }
    if (3) {
      continue;
    }
  }
  4;
}