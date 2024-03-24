function foo(x, y, z) {
  return
}

// Call with no arguments
new foo();

// Arguments passed in contiguous registers
new foo(1, 2, 3);

function bar(x) {
  // Arguments are literals, registers, and globals
  2 + new foo(1, x, foo);
}

function spread(p) {
  return new foo(...p)
}

function argAndSpread(p) {
  return new foo(1, ...p)
}