function test1(x, y) {
  // Accessing arguments
  return x + y;
}

function test2(x) {
  if (x) {
    // Return undefined
    return;
  } else if (x) {
    // Return argument
    return x;
  }

  // Return value
  return 2;
}

function test3() {
  // Implicitly return undefined;
  1;
}

function test4() {
  // Empty function
}