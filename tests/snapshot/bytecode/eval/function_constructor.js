// OPTIONS: --run

var y = 1;
var z = 2;

// Empty
new Function();

// No args
new Function("y + 1");

// With args
new Function("x", "y", "x + y + z");