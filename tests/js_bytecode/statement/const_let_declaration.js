// Global variables
const c1 = 1;
const c2 = 1, y = 2, z = 3;
let l1 = 1;
let l2 = 1, l3 = 2, l4 = 3;

// Always need TDZ check for globals
const c3 = c1 + l1;
const c5 = c4;
const c4 = 1;

function tdzGlobals() {
  // Always need TDZ check for globals
  -c1;
  -l1;

  // Global first loaded to temporary which is where TDZ check is performed
  var local = c1;
}

function noTdzLocals() {
  3;
  const x = 1;
  let y = 2;

  return x + y;
}

function tdzLocals() {
  3;
  const c2 = c1 + l1;
  const c1 = 1;
  let l1 = 1;

  let l2 = c4;
  const c4 = 2;

  // Uses still require TDZ check
  return c1 + l1;
}

function tdzUseInInitializer() {
  3;
  const c1 = c1;
  let l1 = l1;
}

function letNoInitializer() {
  let x;
  x + 2;
}

function assigningConst() {
  const c2 = 1;
  c1 = 2;
  c2 = 3;
}

function prefixUpdateConst() {
  const c2 = 1;
  ++c1;
  ++c2;
}

function postfixUpdateConst() {
  const c2 = 1;
  c1++;
  c2++;
}