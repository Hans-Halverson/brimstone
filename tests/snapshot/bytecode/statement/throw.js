function testThrow() {
  throw 23;
}

function testAbrupt() {
  throw 1;

  // Not emitted
  throw 2;
  5;
}