function use() {}

function logicalAndBoolean() {
  return true && (1 + 2);
}

function logicalAndNotBoolean() {
  return (1 + 2) && (3 + 4);
}

function logicalOrBoolean() {
  return true || (1 + 2);
}

function logicalOrNotBoolean() {
  return (1 + 2) || (3 + 4);
}

function nullishCoalesce() {
  return (1 + 2) ?? (3 + 4);
}

function logicalAndWithTemporary(param) {
  var local = 0;

  // Needs temporary
  param = 1 && 2;
  local = 3 && 4;

  // Does not need temporary
  (5 && 6) * 10;
  use(7 && 8);
}

function localOrWithTemporary(param) {
  var local = 0;

  // Needs temporary
  param = 1 || 2;
  local = 3 || 4;

  // Does not need temporary
  (5 || 6) * 10;
  use(7 || 8);
}

function nullishCoalesceWithTemporary(param) {
  var local = 0;

  // Needs temporary
  param = 1 ?? 2;
  local = 3 ?? 4;

  // Does not need temporary
  (5 ?? 6) * 10;
  use(7 ?? 8);
}