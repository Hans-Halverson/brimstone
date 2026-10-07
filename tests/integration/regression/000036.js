/*---
description: >
  Assignment hazard where the result of an assignment expression is a fixed register that could be
  clobbered by another assignment.
---*/

function simpleAssign() {
  let a = 1;
  return (a = 3) + (a = 4);
}
assert.sameValue(simpleAssign(), 7);

function compoundAssign() {
  let a = 1;
  return (a += 1) + (a = 10);
}
assert.sameValue(compoundAssign(), 12);

function logicalAssign() {
  let a = 0;
  return (a ||= 3) + (a = 10);
}
assert.sameValue(logicalAssign(), 13);

function parameter(a) {
  return (a = 5) + (a = 6, 1);
}
assert.sameValue(parameter(1), 6);

function switchDiscriminant(a) {
  switch (a = 5) {
    case (a = 6, 5):
      return "right";
    default:
      return "wrong";
  }
}
assert.sameValue(switchDiscriminant(1), "right");

function classHeritage() {
  let a;
  class Base {}
  class C extends (a = Base) {
    [(a = null, "m")]() {}
  }
  return Object.getPrototypeOf(C) === Base;
}
assert.sameValue(classHeritage(), true);
