function testAbruptMiddle() {
  1;
  2;
  return 3;
  // Code after abrupt statement is not generated
  4;
}