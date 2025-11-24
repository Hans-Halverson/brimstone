function startScope() {
  switch (0) {
    case 1:
      x + y;
    case 2:
      const x = true;
    case 3:
      let y = false;
  }
}

function alwaysNeedTdzCheck() {
  // TDZ check always needed for def in toplevel switch scope
  switch (0) {
    case 1:
      const x = true;
      x;
    case 2: {
      x;
    }
  }

  // TDZ check not needed for defs in nested scopes
  switch (3) {
    case 1: {
      const x = true;
      x;
    }
  }
}

function captures() {
  switch (0) {
    case 1:
      const x = 1;
    case 2:
      function inner1() {
        return x;
      }
    // Creates a new block context
    case 3: {
      const y = 1;
      function inner2() {
        return x + y;
      }
    }
  }
}

function capturesWithBreak() {
  switch (1) {
    case 1:
      let x = 1;
      function captures() { x }
      break;
  }

  2;
}