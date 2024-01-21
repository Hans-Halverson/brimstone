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
  x.foo = () => {};
  x += () => {};
}

function operatorIdAssign(param) {
  var local = 2;

  // Operator assign to all VM locations - param, local, global
  param += 3;
  local += 4;
  global += 5;
}

function operatorIdAssignReturnValue(param) {
  var local = 2;

  // Assign to all VM locations - param, local, global
  use(param += 3);
  use(local += 4);
  use(global += 5);

  local += param += global += 6;
}

function operatorIdAllOperators(x) {
  x += 1;
  x -= 2;
  x *= 3;
  x /= 4;
  x %= 5;
  x **= 6;
}

function operatorMemberAssign(x) {
  x.foo += 1;
  use(x.foo += 2);

  x.foo += x.bar += 3;

  use(x.foo += x);
}

function operatorMemberAllOperators(x) {
  x.foo += 1;
  x.foo -= 2;
  x.foo *= 3;
  x.foo /= 4;
  x.foo %= 5;
  x.foo **= 6;
}