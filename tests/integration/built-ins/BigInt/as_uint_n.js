/*---
description: BigInt.asUintN arithmetic.
---*/

// Zero-width window always returns 0n.
assert.sameValue(BigInt.asUintN(0, 5n), 0n);
assert.sameValue(BigInt.asUintN(0, -5n), 0n);

// Single-bit window: only 0n and 1n are representable.
assert.sameValue(BigInt.asUintN(1, 0n), 0n);
assert.sameValue(BigInt.asUintN(1, 1n), 1n);
assert.sameValue(BigInt.asUintN(1, -1n), 1n);

// 4-bit window (non-multiple of 8): range [0, 15].
assert.sameValue(BigInt.asUintN(4, 9n), 9n);
assert.sameValue(BigInt.asUintN(4, 15n), 15n);
assert.sameValue(BigInt.asUintN(4, 16n), 0n);
assert.sameValue(BigInt.asUintN(4, -1n), 15n);

// 9-bit window (non-multiple, two bytes): range [0, 511].
assert.sameValue(BigInt.asUintN(9, 5n), 5n);
assert.sameValue(BigInt.asUintN(9, 511n), 511n);
assert.sameValue(BigInt.asUintN(9, 512n), 0n);
assert.sameValue(BigInt.asUintN(9, -1n), 511n);

// 60-bit window (non-multiple, eight bytes): range [0, 2^60-1].
assert.sameValue(BigInt.asUintN(60, 5n), 5n);
assert.sameValue(BigInt.asUintN(60, 2n ** 60n - 1n), 1152921504606846975n);
assert.sameValue(BigInt.asUintN(60, 2n ** 60n), 0n);
assert.sameValue(BigInt.asUintN(60, -1n), 1152921504606846975n);

// 8-bit window: range [0, 255].
assert.sameValue(BigInt.asUintN(8, 5n), 5n);
assert.sameValue(BigInt.asUintN(8, 255n), 255n);
assert.sameValue(BigInt.asUintN(8, 256n), 0n);
assert.sameValue(BigInt.asUintN(8, -1n), 255n);
assert.sameValue(BigInt.asUintN(8, -128n), 128n);

// 24-bit window (byte-aligned, non-power-of-2): range [0, 16777215].
assert.sameValue(BigInt.asUintN(24, 5n), 5n);
assert.sameValue(BigInt.asUintN(24, 16777215n), 16777215n);
assert.sameValue(BigInt.asUintN(24, 16777216n), 0n);
assert.sameValue(BigInt.asUintN(24, -1n), 16777215n);

// 64-bit window: range [0, 2^64-1].
assert.sameValue(BigInt.asUintN(64, 5n), 5n);
assert.sameValue(BigInt.asUintN(64, 2n ** 64n - 1n), 18446744073709551615n);
assert.sameValue(BigInt.asUintN(64, 2n ** 64n), 0n);
assert.sameValue(BigInt.asUintN(64, -1n), 18446744073709551615n);
assert.sameValue(BigInt.asUintN(64, -(2n ** 63n)), 9223372036854775808n);

// 128-bit window: range [0, 2^128-1].
assert.sameValue(BigInt.asUintN(128, 5n), 5n);
assert.sameValue(BigInt.asUintN(128, -1n), 340282366920938463463374607431768211455n);
assert.sameValue(BigInt.asUintN(128, 2n ** 128n), 0n);
assert.sameValue(BigInt.asUintN(128, 2n ** 200n + 1n), 1n);
