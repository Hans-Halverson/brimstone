function foo(x, y, z) {
  return
}

// Call with no arguments
foo();

// Arguments passed in contiguous registers
foo(1, 2, 3);

function bar(x) {
  // Arguments are literals, registers, and globals
  2 + foo(1, x, foo);
}