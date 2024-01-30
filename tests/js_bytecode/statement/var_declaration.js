// Global variables
var x = 1;
var x = 1, y = 2, z = 3;

function testLocals() {
  3;
  var x = 1;
  var y = 2;

  return x + y;
}

function testDuplicates() {
  3;
  // Duplicate vars reference the same local binding
  var x = 1;
  var x = 2;

  return x;
}

function testLoadFrom(arg) {
  // Load from temporary
  var x1 = 1;
  var x2 = 1 + 2 + 3;
  // Load from argument
  var x3 = arg;
  // Load from local
  var x4 = y;
}