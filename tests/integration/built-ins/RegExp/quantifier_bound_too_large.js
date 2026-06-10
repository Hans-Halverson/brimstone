/*---
description: Quantifiers whose bounds are larger than the max string length.
---*/

// Optional repetitions with out of range max can still match
assert.compareArray(/ax{0,100000000000000}b/.exec("axxb"), ['axxb']);

// Required repetitions with out of range max never match
assert.sameValue(/ax{100000000000000,100000000000001}b/.exec("axxb"), null);

// Optional repetitions that don't always consume can still match
assert.compareArray(/a(?:x|){0,100000000000000}b/.exec("axxb"), ['axxb']);

// Required repetitions that don't always consume will match normally but overflow stack
assert.throws(RangeError, () => /a(?:x|){100000000000000,100000000000001}b/.exec("axxb"));