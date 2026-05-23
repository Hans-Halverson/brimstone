/*---
description: Storing i64::MIN into a BigInt64Array crashed from integer overflow.
---*/

const a = new BigInt64Array(1);

a[0] = -9223372036854775808n;
assert.sameValue(a[0], -9223372036854775808n);

a[0] = 9223372036854775808n;
assert.sameValue(a[0], -9223372036854775808n);
