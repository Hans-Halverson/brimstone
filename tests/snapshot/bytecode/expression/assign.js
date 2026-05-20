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

function testNamedMemberAssign(x, y) {
  x.foo = 1;
  use(x.foo = 2);

  x.foo = x.bar = 3;

  use(x.foo = x);

  // Do not clobber destination with intermediate value
  y = x.foo = 4;
}

function testComputedMemberAssign(x, y) {
  x['foo'] = 1;
  use(x['foo'] = 2);

  x['foo'] = x['bar'] = 3;

  use(x['foo'] = x);

  // Do not clobber destination with intermediate value
  y = x['foo'] = 4;
}

({
  testSuperNamedMemberAssign(x) {
    super.foo = 1;
    use(super.foo = 2);
  
    super.foo = super.bar = 3;
  
    use(super.foo = x);
  
    // Do not clobber destination with intermediate value
    x = super.foo = 4;
  },

  testSuperComputedMemberAssign(x) {
    super['foo'] = 1;
    use(super['foo'] = 2);
  
    super['foo'] = super['bar'] = 3;
  
    use(super['foo'] = x);
  
    // Do not clobber destination with intermediate value
    x = super['foo'] = 4;
  },
});

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

  // Do not clobber destination with intermediate value
  local += param += global += 6;
}

function objectDestructuringAssign(param) {
  var local, a;
  ({a} = 1);
  param = ({a} = 2);
  local = ({a} = 3);
  global = ({a} = 4);
}

function operatorIdAllOperators(x) {
  x += 1;
  x -= 2;
  x *= 3;
  x /= 4;
  x %= 5;
  x **= 6;
  x &= 7;
  x |= 8;
  x ^= 9;
  x <<= 10;
  x >>= 11;
  x >>>= 12;
}

function operatorMemberAssign(x, y) {
  x.foo += 1;
  use(x.foo += 2);

  x.foo += x.bar += 3;

  use(x.foo += x);

  // Do not clobber destination with intermediate value
  y = x.foo += 4;
}

function operatorMemberAllOperators(x) {
  x.foo += 1;
  x.foo -= 2;
  x.foo *= 3;
  x.foo /= 4;
  x.foo %= 5;
  x.foo **= 6;
  x.foo &= 7;
  x.foo |= 8;
  x.foo ^= 9;
  x.foo <<= 10;
  x.foo >>= 11;
  x.foo >>>= 12;
}

function tdzCheck() {
  let x = 0;
  function inner() { x }

  // Needs TDZ check
  x = 1;
  ({x} = 2);
}