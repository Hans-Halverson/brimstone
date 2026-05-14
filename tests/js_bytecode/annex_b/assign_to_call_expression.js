function f() {}

function assignmentExpression() {
  f() = 1 + 2;
}

function updateExpression() {
  f()++;
  f()--;
  ++f();
  --f();
}

function forInInitializer() {
  for (f() in {}) {}
}

function forOfInitializer() {
  for (f() of []) {}
}