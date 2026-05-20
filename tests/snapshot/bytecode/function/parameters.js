function destructuring({a, b}, {c: d, e: f = 1}) {}

function defaultValues(a, b = 1, c = 2, d = b) {}

function rest(a, b = 1, ...c) {}

function restDestructuring(a, b, ...{c}) {}

function captured({a}, b = 1, ...c) {
  function inner() {
    return a + b + c;
  }
}

function tdz(a = b, b) {
  a + b;
}

function tdzCaptured(a = b, b) {
  a + b;
  function inner() {
    return a + b;
  }
}

function shadowed(x, x, x) {
  return x;
}