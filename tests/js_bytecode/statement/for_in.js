function basicVar() {
  1;
  for (var i in {}) {
    2;
  }
  3;
}

function basicConstLet() {
  1;
  for (const i in {}) {
    2;
  }
  3;
  for (let i in {}) {
    3;
  }
  4;
}

function testBreakAndContinue() {
  1;
  for (var i in {}) {
    if (2) {
      break;
    }
    if (3) {
      continue;
    }
  }
  4;
}

function lhsIdentifier(param) {
  1;
  for (param in {}) {
    param;
  }
  2;
}

function lhsMember(x) {
  1;
  for (x.foo in {}) {
    2;
  }
  3;
  for (x[1] in {}) {
    4;
  }
  5;
}

function lhsDestructuring(param) {
  var a, c, e;
  1;
  for (var {a, b: c, d: e = 1} in {}) {
    2;
  }
  3;
  for ({a, b: c, d: e = 1} in {}) {
    4;
  }
  5;
}

function lhsTdz() {
  // No TDZ check due to declaration
  for (let x in {}) {
    function inner() { return x; }
  }

  // TDZ check needed due to assignment
  let y = 1;
  for (y in {}) {
    function inner() { return y; }
  }
}