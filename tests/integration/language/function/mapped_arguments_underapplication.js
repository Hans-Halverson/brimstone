/*---
description: >
  A mapped arguments object only maps parameters that were supplied an argument. Indices past the
  number of supplied arguments are not properties of the arguments object.
flags: [noStrict]
---*/

function underapplied(a, b) {
  b = 5;
  assert.sameValue(arguments.length, 1);
  assert.sameValue(1 in arguments, false);
  assert.sameValue(arguments[1], undefined);

  arguments[1] = 9;
  assert.sameValue(b, 5);
  assert.sameValue(arguments[1], 9);
}
underapplied(7);

function noArgumentsSupplied(a) {
  a = 100;
  assert.sameValue(arguments.length, 0);
  assert.sameValue(arguments[0], undefined);
  assert.sameValue(Object.keys(arguments).length, 0);
}
noArgumentsSupplied();

function suppliedAreStillMapped(a, b) {
  a = "A";
  assert.sameValue(arguments[0], "A");

  arguments[1] = "B";
  assert.sameValue(b, "B");
}
suppliedAreStillMapped(1, 2);

function extraArgumentsAreNotMapped(a) {
  a = "A";
  assert.sameValue(arguments[0], "A");

  arguments[1] = "B";
  assert.sameValue(arguments[1], "B");
  assert.sameValue(arguments.length, 2);
}
extraArgumentsAreNotMapped(1, 2);
