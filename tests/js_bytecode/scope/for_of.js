function startScope() {
  for (const x of []) {
    x + inner;
    const inner = 0;
  }
}

function captures() {
  // For initializer in outer scope
  for (const x of []) {
    // Body in separate inner scope
    const y = 2;
    function inner() {
      return x + y;
    }
  }
}

function captureRhsInTdz() {
  for (const x of x) {
    function inner() {
      return x;
    }
  }
}

function noCaptureRhsInTdz() {
  for (const x of x) {
    1;
  }
}