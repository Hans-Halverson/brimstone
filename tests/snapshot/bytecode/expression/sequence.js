function use() {}

function testAllExpressionsEvaluated() {
  (1, 2, 3);
}

function testLastExpressionIsReturned() {
  use((1, 2, 3));
  return (1, 2, 3);
}

function testOnlyLastExpressionResultIsUsed() {
  var x = 1;
  var y = 2;
  var z = 3;
  (x++, y++, z++);
}