function use() {}

function testAllExpressionsEvaluated() {
  (1, 2, 3);
}

function tesLastExpressionIsReturned() {
  use((1, 2, 3));
  return (1, 2, 3);
}