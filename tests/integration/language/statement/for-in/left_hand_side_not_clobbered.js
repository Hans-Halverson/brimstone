/*---
description: The left hand side of a for-in loop is not clobbered if no iterations occur or after the final iteration.
---*/

var a1 = 10;
for (a1 in {});
assert.sameValue(a1, 10);

var a2 = 20;
for (a2 in { prop: 1 });
assert.sameValue(a2, 'prop');

(function() {
  var a3 = 30;
  for (a3 in {});
  assert.sameValue(a3, 30);
})();

(function() {
  var a4 = 40;
  for (a4 in { prop: 1 });
  assert.sameValue(a4, 'prop');
})();