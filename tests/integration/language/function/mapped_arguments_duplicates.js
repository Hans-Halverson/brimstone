/*---
description: >
  Duplicate parameter names share a single binding which refers to the last parameter with that
  name. Only that last index is mapped in a mapped arguments object.
flags: [noStrict]
---*/

function duplicates(x, x) {
  assert.sameValue(x, 2);
  assert.sameValue(arguments[0], 1);
  assert.sameValue(arguments[1], 2);

  x = "last";
  assert.sameValue(arguments[0], 1);
  assert.sameValue(arguments[1], "last");

  arguments[0] = "zero";
  assert.sameValue(x, "last");

  arguments[1] = "one";
  assert.sameValue(x, "one");
}
duplicates(1, 2);

function nonAdjacentDuplicates(x, y, x) {
  assert.sameValue(x, 3);
  assert.sameValue(y, 2);

  arguments[2] = "third";
  assert.sameValue(x, "third");

  arguments[0] = "first";
  assert.sameValue(x, "third");
}
nonAdjacentDuplicates(1, 2, 3);

function withoutArgumentsObject(x, x) {
  return x;
}
assert.sameValue(withoutArgumentsObject(1, 2), 2);

function underappliedDuplicates(x, x) {
  assert.sameValue(x, undefined);
  assert.sameValue(arguments[0], 1);
  assert.sameValue(arguments.length, 1);

  x = "assigned";
  assert.sameValue(arguments[0], 1);
}
underappliedDuplicates(1);
