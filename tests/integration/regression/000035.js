/*---
description: >
  Assignment hazards in switch discriminant and case test expressions, which are evaluated together.
---*/

function nestedAssign(a) {
  switch (a) {
    case (a = 5, 5):
      return "wrong";
    default:
      return "right";
  }
}
assert.sameValue(nestedAssign(1), "right");

function topLevelAssign(a) {
  switch (a) {
    case a = 5:
      return "wrong";
    case 1:
      return "right";
  }
}
assert.sameValue(topLevelAssign(1), "right");

function topLevelUpdate(a) {
  switch (a) {
    case a++:
      return "first";
    case 1:
      return "second";
    default:
      return "none";
  }
}
assert.sameValue(topLevelUpdate(1), "first");

function localBinding() {
  let a = 1;
  switch (a) {
    case (a = 2):
      return "wrong";
    case 1:
      return "right:" + a;
  }
}
assert.sameValue(localBinding(), "right:2");
