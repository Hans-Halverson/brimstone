// Let declarations
let a = 1;
let [a2] = 1;
let {a3} = 1;
let static = 1;
let async = 1;
let await = 1;
let yield = 1;

// Let at start of expression
let;
let + 1;
let(1);

// ASI after let
let
1;

// Let declaration in for
for (let a = 1;;) {}
for (let a in b) {}

// Let at start of expression
for (let;;) {}
for (let + 1;;) {}
for (let in b) {}