function startScopeNoTDZInOuter() {
  for (let x = 1; x < 10; x++) {
    x + inner;
    const inner = 0;
  }

  for (let y = 1; y < 10; y++) {
    y + inner;
    const inner = 0;
  }
}

function startScopeTDZInOuter() {
  for (let x = x; x < 10; x++) {
    1;
    inner;
    2;
    const inner = 0;
    3;
  }
}

function captures() {
  // For initializer in outer scope
  for (let x = 1; x < 10; x += 1) {
    // Body in separate inner scope
    const y = 2;
    function inner() {
      return x + y;
    }
  }
}
