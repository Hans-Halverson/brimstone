class C1 {}

function tdzGlobals() {
  // Always need TDZ check for globals
  return C1;
}

function noTdzLocals() {
  class C {}
  return C;
}

function tdzLocals() {
  C;
  class C {}
}

function tdzUseInSuperClass() {
  class C extends C {}
  (class C extends C {});
}