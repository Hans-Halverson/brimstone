function startScope() {
  for (const x in {}) {
    x + inner;
    const inner = 0;
  }
}

function captures() {
  // For initializer in outer scope
  for (const x in {}) {
    // Body in separate inner scope
    const y = 2;
    function inner() {
      return x + y;
    }
  }
}

function captureRhsInTdz() {
  for (const x in x) {
    function inner() {
      return x;
    }
  }
}

function noCaptureRhsInTdz() {
  for (const x in x) {
    1;
  }
}