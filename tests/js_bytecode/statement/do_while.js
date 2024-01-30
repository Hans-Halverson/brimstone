function testBasic() {
  1;
  do {
    2;
  } while (3);
  4;
}

function testBooleanTest() {
  1;
  do {
    2;
  } while (true);
  3;
}

function testAbruptBody() {
  1;
  do {
    2;
    return;
    3;
    // Test is still emitted
  } while (true);
  4;
}

function testBreakAndContinue() {
  1;
  do {
    if (2) {
      break;
    }
    if (3) {
      continue;
    }
  } while (true);
  4;
}