/*---
description: >
  Reading and writing beyond the bounds of a mapped arguments object should not read or write mapped
  parameters.
---*/

// Reading and writing past end of a small number of arguments (inline bitmap)
function smallRead(a, b) {
  return arguments[32];
}
assert.sameValue(smallRead(1, 2), undefined);

function smallWrite(a, b) {
  arguments[32] = 99;
  return arguments[32];
}
assert.sameValue(smallWrite(1, 2), 99);

// Reading and writing past end of a large number of arguments (heap allocated bitmap). At least 33
// arguments must be supplied so that the bitmap does not fit inline in a smi.
function largeRead(
  a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,
  a26,a27,a28,a29,a30,a31,a32,a33,a34,
) {
  return arguments[100];
}
assert.sameValue(largeRead(
  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
  26,27,28,29,30,31,32,33,34,
), undefined);

function largeWrite(
  a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,
  a26,a27,a28,a29,a30,a31,a32,a33,a34,
) {
  arguments[100] = 99;
  // The out of bounds write must not have clobbered any mapped parameter
  return [arguments[100], a1, a34];
}
var largeResult = largeWrite(
  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
  26,27,28,29,30,31,32,33,34,
);
assert.sameValue(largeResult[0], 99);
assert.sameValue(largeResult[1], 1);
assert.sameValue(largeResult[2], 34);