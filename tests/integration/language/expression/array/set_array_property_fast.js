/*---
description: Array literals use SetArrayProperty in both the fast and slow paths.
includes: [compareArray.js]
---*/

// Fast path appends values and holes within the initial capacity
var holes = [1, , 3];
assert.sameValue(holes.length, 3);
assert.sameValue(1 in holes, false);
assert.compareArray(holes, [1, undefined, 3]);

// Spread exceeds the initial capacity, slow path grows the array then fast path resumes
assert.compareArray([0, ...[1, 2, 3]], [0, 1, 2, 3]);

// Rest element exceeds the initial capacity of the rest array
var [...rest] = [0, 1, 2, 3, 4];
assert.compareArray(rest, [0, 1, 2, 3, 4]);

// Growing a long array of holes transitions to sparse, slow path stores values and holes
var sparse = eval("[" + ",".repeat(1100) + "...[1, 2, 3], , ]");
assert.sameValue(sparse.length, 1104);
assert.compareArray(Object.keys(sparse), ["1100", "1101", "1102"]);
assert.sameValue(sparse[1102], 3);
