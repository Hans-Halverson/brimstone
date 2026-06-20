/*---
description: BigInt.asIntN arithemtic.
---*/

// Zero-width window always returns 0n.
assert.sameValue(BigInt.asIntN(0, 5n), 0n);
assert.sameValue(BigInt.asIntN(0, -5n), 0n);

// Single-bit window: only 0n and -1n are representable.
assert.sameValue(BigInt.asIntN(1, 0n), 0n);
assert.sameValue(BigInt.asIntN(1, 1n), -1n);
assert.sameValue(BigInt.asIntN(1, -1n), -1n);

// 4-bit window (non-multiple of 8): signed range [-8, 7].
assert.sameValue(BigInt.asIntN(4, 7n), 7n);
assert.sameValue(BigInt.asIntN(4, 8n), -8n);
assert.sameValue(BigInt.asIntN(4, -1n), -1n);
assert.sameValue(BigInt.asIntN(4, -9n), 7n);

// 12-bit window (non-multiple, two bytes): signed range [-2048, 2047].
assert.sameValue(BigInt.asIntN(12, 5n), 5n);
assert.sameValue(BigInt.asIntN(12, 2048n), -2048n);
assert.sameValue(BigInt.asIntN(12, -1n), -1n);
assert.sameValue(BigInt.asIntN(12, -2049n), 2047n);

// 60-bit window (non-multiple, eight bytes): signed range [-2^59, 2^59-1].
assert.sameValue(BigInt.asIntN(60, 5n), 5n);
assert.sameValue(BigInt.asIntN(60, 2n ** 59n), -576460752303423488n);
assert.sameValue(BigInt.asIntN(60, -1n), -1n);

// 8-bit window: signed range [-128, 127].
assert.sameValue(BigInt.asIntN(8, 5n), 5n);
assert.sameValue(BigInt.asIntN(8, 128n), -128n);
assert.sameValue(BigInt.asIntN(8, -1n), -1n);
assert.sameValue(BigInt.asIntN(8, -129n), 127n);

// 24-bit window (byte-aligned, non-power-of-2): signed range [-8388608, 8388607].
assert.sameValue(BigInt.asIntN(24, 5n), 5n);
assert.sameValue(BigInt.asIntN(24, 8388608n), -8388608n);
assert.sameValue(BigInt.asIntN(24, -1n), -1n);

// 64-bit window: signed range [-2^63, 2^63-1].
assert.sameValue(BigInt.asIntN(64, 5n), 5n);
assert.sameValue(BigInt.asIntN(64, -1n), -1n);
assert.sameValue(BigInt.asIntN(64, 2n ** 63n - 1n), 9223372036854775807n);
assert.sameValue(BigInt.asIntN(64, 2n ** 63n), -9223372036854775808n);
assert.sameValue(BigInt.asIntN(64, -(2n ** 63n) - 1n), 9223372036854775807n);

// 128-bit window: signed range [-2^127, 2^127-1].
assert.sameValue(BigInt.asIntN(128, 5n), 5n);
assert.sameValue(BigInt.asIntN(128, -1n), -1n);
assert.sameValue(BigInt.asIntN(128, 2n ** 127n), -170141183460469231731687303715884105728n);
assert.sameValue(BigInt.asIntN(128, 2n ** 200n + 1n), 1n);
