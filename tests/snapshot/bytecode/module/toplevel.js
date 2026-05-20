// Regular lexical scope - all variables in registers
var x1 = 1;
let x2 = 2;
const x3 = 3;

// Lexical declaration
function toplevelMethod() {}

{
  function inBlock() {}
}

100 + x1;

function testToplevelBinding() {
  x1 + x2 + x3 + toplevelMethod;
}