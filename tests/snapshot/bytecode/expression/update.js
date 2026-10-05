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

  // Unused dest
  ++x.prop;
  ++x[0];
  ++x[y];
}

function postfixMember(x, y) {
  // Temporary dest
  -(x.prop++);
  -(x[0]++);

  // Fixed dest
  y = x.prop++;

  // Unused dest
  x.prop++;
  x[0]++;
  x[y]++;
}

function postfixMemberUnused(x, y) {
  // Destination is unused so emitted as prefix update
  x.prop++;
  x[0]++;
  x[y]++;
}

({
  prefixSuperMember(x, y) {
    // Temporary dest
    -(++super.prop);
    -(++super[0]);

    // Fixed dest
    x = ++super.prop;

    // Unused dest
    ++super.prop;
    ++super[0];
    ++super[y];
  },
  
  postfixSuperMember(x, y) {
    // Temporary dest
    -(super.prop++);
    -(super[0]++);

    // Fixed dest
    x = super.prop++;
  
    // Unused dest
    super.prop++;
    super[0]++;
    super[y]++;
  },
});

function decrement(param) {
  --param;
  -(param--);
  --param.prop;
  -(param.prop--);
}