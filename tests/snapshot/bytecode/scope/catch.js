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

function capturesWithCapturedParam() {
  try {
    1;
  } catch (x) {
    const y = 1;
    function inner() {
      return x + y;
    }
  }
}

function capturesNoParam() {
  try {
    1;
  } catch {
    const x = 1;
    function inner() {
      return x;
    }
  }
}

function capturesWithUncapturedParam() {
  try {
    1;
  } catch (x) {
    const y = 1 + x;
    function inner() {
      return y;
    }
  }
}