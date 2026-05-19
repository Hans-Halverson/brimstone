/*---
description: Crash due to overflow in number of `String.raw` arguments.
---*/

assert.throws(TypeError, () => String.raw());