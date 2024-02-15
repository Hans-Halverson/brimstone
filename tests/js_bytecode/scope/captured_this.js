// Reference to global `this` is direct
var v1 = this;

{
  // Reference to captured global `this`
  (() => 1 + this);
}

function capturedFunctionThis() {
  // Direct reference to function's `this`
  var v1 = this;

  // Reference to captured function `this`
  (() => 2 + this);

  // Nested reference
  (() => 3 + (() => 4 + (() => 5 + this)));
}

function noCapture() {
  var v1 = this;
  function inner() {
    var v2 = this;
  }
}