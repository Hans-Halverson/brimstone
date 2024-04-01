function basic(x) {
  1;
  with (x) {
    2;
  }
  3;
}

function withVars(x) {
  with (x) {
    var y = 1;
    y + 2;
    y = 3;
  }

  y + 4;
  y = 5;
}

function withVarShadowsVar(x) {
  var y = 0;

  with (x) {
    var y = 1;
    y = 2;
  }

  y = 3;
}

function withVarShadowsArg(x, p) {
  with (x) {
    var p = 1;
  }

  p = 2;
}

function withVarShadowsArguments(x) {
  with (x) {
    var arguments = 1;
  }

  arguments = 2;
}