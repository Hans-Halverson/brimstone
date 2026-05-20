function unaryMinus() {
  // Not a unary expression, inlined as negative number literal
  -1;

  // Unary minus expression
  var x = 1;
  -x;
  -(-x);
}

function unaryPlus() {
  +1;
  +x;
  +(+x);
}

function logicalNot() {
  !1;
  !true;
  !!false;
}

function bitwiseNot() {
  ~1;
  ~~2;
}

function _void() {
  void 1;
}