/*---
description: Assignment hazards in for-each statements with var declaration initializers.
---*/

function forOfVar(a) {
  for (var [x = a + (a = 5)] of [[]]) return x;
}
assert.sameValue(forOfVar(1), 6);

function forOfLet(a) {
  for (let { x = a + (a = 5) } of [{}]) return x;
}
assert.sameValue(forOfLet(1), 6);

function forOfConst() {
  let a = 1;
  for (const [x = a + (a = 5)] of [[]]) return x;
}
assert.sameValue(forOfConst(), 6);

function forInVar(a) {
  for (var [k, x = a + (a = 5)] in { k: 0 }) return k + x;
}
assert.sameValue(forInVar(1), "k6");

function forInLet(a) {
  for (let [k, x = a + (a = 5)] in { k: 0 }) return k + x;
}
assert.sameValue(forInLet(1), "k6");
