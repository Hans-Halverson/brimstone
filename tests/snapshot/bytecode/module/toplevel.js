// Regular lexical scope - captured and stored in scope
var x1 = 1;
let x2 = 2;
const x3 = 3;

// Regular lexical scope - not captured and stored in registers
var x4 = 4;
let x5 = 5;
const x6 = 6;

// Lexical declaration
function toplevelNonCapturedFunction() {}
function toplevelCapturedFunction() {}

{
  function inBlock() {}
}

100 + x1;

function testToplevelBinding() {
  x1 + x2 + x3 + toplevelCapturedFunction;
}