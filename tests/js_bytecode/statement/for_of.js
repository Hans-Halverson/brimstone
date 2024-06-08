function basicVar() {
  1;
  for (var i of []) {
    2;
  }
  3;
}

function basicConstLet() {
  1;
  for (const i of []) {
    2;
  }
  3;
  for (let i of []) {
    3;
  }
  4;
}

function testAllAbruptPaths(p) {
  1;
  for (var i of []) {
    if (2) {
      break;
    }
    if (3) {
      continue;
    }
    if (4) {
      return p;
    }
  }
  5;
}

function lhsIdentifier(param) {
  1;
  for (param of []) {
    param;
  }
  2;
}

function lhsMember(x) {
  1;
  for (x.foo of []) {
    2;
  }
  3;
  for (x[1] of []) {
    4;
  }
  5;
}

function lhsDestructuring(param) {
  var a, c, e;
  1;
  for (var {a, b: c, d: e = 1} of []) {
    2;
  }
  3;
  for ({a, b: c, d: e = 1} of []) {
    4;
  }
  5;
}

function lhsTdz() {
  // No TDZ check due to declaration
  for (let x of []) {
    function inner() { return x; }
  }

  // TDZ check needed due to assignment
  let y = 1;
  for (y of []) {
    function inner() { return y; }
  }
}

async function forAwaitOf() {
  1;
  for await (var x of []) {
    2;
  }
  3;
}