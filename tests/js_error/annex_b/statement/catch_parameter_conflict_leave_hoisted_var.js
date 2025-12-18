function test() {
  // Conflict with hoisted var in scope outside catch, even though var does not conflict within the
  // catch body itself.
  const foo = 1;

  try {
    2;
  } catch (foo) {
    var foo = 4;
  }
}