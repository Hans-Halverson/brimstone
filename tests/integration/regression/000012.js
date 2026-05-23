/*---
description: Math.abs(i32::MIN) overflowed.
---*/

assert.sameValue(Math.abs(-2147483648), 2147483648);
