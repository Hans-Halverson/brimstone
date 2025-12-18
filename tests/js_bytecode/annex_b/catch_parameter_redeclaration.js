function test() {
  // Reference hoisted variable
  foo = 1;

  try {
    2;
  } catch (foo) {
    3;
    
    // Store to catch paramter
    var foo = 4;

    // Load catch parameter
    foo = 5;

    6;
  }

  // Reference hoisted variable
  foo = 6;
}