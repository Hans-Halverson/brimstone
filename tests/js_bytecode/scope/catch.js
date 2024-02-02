function startScope() {
  try {
    1;
  } catch ({a = b, b}) {
    // TDZ check needed for a catch parameter and a value in the block
    2;
    x;
    const x = 1;
  }
}

startScope();