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