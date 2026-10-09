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

  // Destination is NewTemporary
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

  // Destination is NewTemporary
  use(param++);
  use(local++);
  use(global++);
}

function postfixIdUnused(param) {
  var local = 1;

  // Destination is unused so emitted as prefix update
  param++;
  local++;
  global++;
}

function prefixMember(x, y) {
  // Temporary dest
  -(++x.prop);
  -(++x[0]);

  // Fixed dest
  y = ++x.prop;
}

function postfixMember(x, y) {
  // Temporary dest
  -(x.prop++);
  -(x[0]++);

  // Fixed dest
  y = x.prop++;
}

function postfixMemberUnused(x) {
  // Destination is unused so emitted as prefix update
  x.prop++;
  x[0]++;
}

({
  prefixSuperMember(x) {
    // Temporary dest
    -(++super.prop);
    -(++super[0]);
  
    // Fixed dest
    x = ++super.prop;
  },
  
  postfixSuperMember(x) {
    // Temporary dest
    -(super.prop++);
    -(super[0]++);
  
    // Fixed dest
    x = super.prop++;
  },
});

function decrement(param) {
  --param;
  -(param--);
  --param.prop;
  -(param.prop--);
}

function inTernaryAssignedToSelf(x, y) {
  x = y ? x++ : 0;
  x = y ? ++x : 0;
  
  // Sloppy reassignment of function expression name
  (function f(x, y) {
    x = y ? f++ : 0;
    x = y ? ++f : 0;
  });
}