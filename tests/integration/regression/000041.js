/*---
description: Update expressions were sometimes ignoring a fixed destination register.
flags: [noStrict]
---*/

// Postfix update whose fixed destination is the binding's own register
function ownRegister() {
  let d = 1;
  d = true ? d++ : 0;
  return d;
}
assert.sameValue(ownRegister(), 1);

// Sloppy-mode reassignment of a function expression's own name is a no-op
assert.sameValue((function g() { let x = 0; x = true ? g++ : 0; return x; })(), NaN);
assert.sameValue((function g() { let x = 0; x = true ? ++g : 0; return x; })(), NaN);

// The leaked temporary sat between the call's argument registers, shifting later arguments
function leakedTemporary() {
  function f(x, y, z) {
    return [x, y, z];
  }
  return (function g() { return f(true ? g++ : 0, 1, 2); })();
}
var args = leakedTemporary();
assert.sameValue(args[0], NaN);
assert.sameValue(args[1], 1);
assert.sameValue(args[2], 2);

// Already correct because plain assignment uses the returned register; the fix adds a mov here
function plainAssign() {
  let d = 1;
  d = d++;
  return d;
}
assert.sameValue(plainAssign(), 1);