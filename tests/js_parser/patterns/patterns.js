// Identifier patterns
var x = 1;

// Assignment patterns
function test(x = 1, y = 2) {}

// Patterns in all locations
var [a] = 1;

function foo([a]) {}

for (var [a] of 1) {}

try {} catch ([a]) {}

for ([a] of 1) {}

[a] = 1;