function namedMethod() {
  return { a() {}, b(c) { return 2 } };
}

function computedMethod() {
  return { [1]() {} };
}
