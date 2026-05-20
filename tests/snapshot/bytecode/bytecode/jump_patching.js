function f() {
  return;
}

function testShortPatchCondJump(x) {
    // Short jumps are inlined as immediate relative offsets
    if (x) {
      return 1;
    }

    return 2;
}

function testLongPatchCondJump(x) {
  // Longer jumps are placed in constant table
  if (x) {
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    return 1;
  }

  return 2;
}

function testShortPatchJump(x) {
  // Short jumps are inlined as immediate relative offsets
  if (x) {
    1;
  } else {
    2;
  }

  return 3;
}

function testShortPatchJump2(x) {
  // Longer jumps are placed in constant table
  if (x) {
    1;
  } else {
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  }

  return 2;
}