/*---
description: Lookbehind at the start of a two-byte string does not overflow.
---*/

assert(/(?<=a)b/u.test("abc😀"));
