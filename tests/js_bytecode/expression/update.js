var global = 0;

function use() {}

function prefixIdAnyDest(param) {
  var local = 1;

  // Destination is Any
  -(++param);
  -(++local);
  -(++global);
}

function prefixIdFixedDest(param) {
  var local = 1;

  // Destination is Fixed
  local = ++param;
  local = ++local;
  local = ++global;
}

function prefixIdNewTemporaryDest(param) {
  var local = 1;

  // Desination is NewTemporary
  use(++param);
  use(++local);
  use(++global);
}

function postfixIdAnyDest(param) {
  var local = 1;

  // Destination is Any
  -(param++);
  -(local++);
  -(global++);
}

function postfixIdFixedDest(param) {
  var local = 1;

  // Destination is Fixed
  local = param++;
  local = local++;
  local = global++;
}

function postfixIdNewTemporaryDest(param) {
  var local = 1;

  // Desination is NewTemporary
  use(param++);
  use(local++);
  use(global++);
}

function prefixMember(x) {
  -(++x.prop);
  -(++x[0]);
}

function postfixMember(x) {
  -(x.prop++);
  -(x[0]++);
}

function decrement(param) {
  --param;
  param--;
  --param.prop;
  param.prop--;
}