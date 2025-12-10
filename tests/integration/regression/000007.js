/*---
description: Crash due to overflow during date calculations.
---*/

assert.sameValue(new Date(1.0e+308, 0).toString(), "Invalid Date");
assert(Number.isNaN(Date.UTC(1.0e+308)));
assert(Number.isNaN(new Date().setFullYear(1.0e+308)));
assert(Number.isNaN(new Date().setUTCFullYear(1.0e+308)));