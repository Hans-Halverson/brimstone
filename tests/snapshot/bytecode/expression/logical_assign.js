var global = 1;

function logicalAnd(param) {
  var local = 0;

  // Needs temporary
  param &&= 1;
  local &&= 2;

  // Does not need temporary
  global &&= 3;
  param.foo &&= 4;
  use(param &&= 5);
  use(global &&= param);
}

function localOr(param) {
  var local = 0;

  // Needs temporary
  param ||= 1;
  local ||= 2;

  // Does not need temporary
  global ||= 3;
  param.foo ||= 4;
  use(param ||= 5);
  use(global ||= param);
}

function nullishCoalesce(param) {
  var local = 0;

  // Needs temporary
  param ??= 1;
  local ??= 2;

  // Does not need temporary
  global ??= 3;
  param.foo ??= 4;
  use(param ??= 5);
  use(global ??= param);
}

function testAssignDest(param) {
  var local = 0;

  // Do not clobber destination with intermediate value
  local = param &&= 1;
  -(local.foo = param &&= 2);

  return global &&= param;
}

function named() {
  var x = 1;
  
  // Named
  x &&= () => {};
  x ||= () => {};
  x ??= () => {};
}