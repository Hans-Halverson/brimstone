var global = 1;

function use() {}

function testIdAssign(param) {
  var local = 2;

  // Assign to all VM locations - param, local, global
  param = 3;
  local = 4;
  global = 5;
}

function testIdAssignReturnValue(param) {
  var local = 2;

  // Assign to all VM locations - param, local, global
  use(param = 3);
  use(local = 4);
  use(global = 5);

  local = param = global = 6;
}

function testMemberAssign(x) {
  x.foo = 1;
  use(x.foo = 2);

  x.foo = x.bar = 3;

  use(x.foo = x);
}

function named() {
  var x = 1;
  
  // Named
  x = () => {};

  // Not named
  (x) = () => {};
}