function basic() {
  (function foo() { return 1 }) + 2;

  (function () { return 3 }) + 4;

  var x = function () { return 5 };
}

function named() {
  // Named
  var x = function() {}
  var y = 1;

  // Named
  y = function() {};

  // Not named
  var z = function fnName() {};
}

function namedCaptured() {
  var x = function inner() {
    () => { return inner };
    return inner;
  }
}

function namedVarOverwrite() {
  (function inner() {
    var inner;
  })
}

function namedLexOverwrite() {
  (function inner() {
    let inner;
  })
}

function namedSloppyReassign() {
  (function inner() {
    inner = 1;
    inner++;
    ++inner;
  });

  (function innerCaptured() {
    innerCaptured = 1;
    innerCaptured++;
    ++innerCaptured;
    () => innerCaptured;
  });
}

function namedStrictReassign() {
  (function inner() {
    "use strict";
    inner = 1;
    inner++;
    ++inner;
  });

  (function innerCaptured() {
    "use strict";
    innerCaptured = 1;
    innerCaptured++;
    ++innerCaptured;
    () => innerCaptured;
  });
}