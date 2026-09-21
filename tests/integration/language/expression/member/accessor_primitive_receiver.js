/*---
description: >
   Primitive recievers are coerced to objects for property lookup, but the original primitive
   receiver is used as the `this` value within accessors.
---*/

var seen;

Object.defineProperty(Number.prototype, "method", {
  get: function () { "use strict"; return typeof this; },
  set: function (value) { "use strict"; seen = typeof this; },
  configurable: true,
});

var key = "method";

// GetNamedProperty
assert.sameValue((1).method, "number");

// GetProperty
assert.sameValue((1)[key], "number");

// SetNamedProperty
seen = undefined;
(1).method = 1;
assert.sameValue(seen, "number");

// SetProperty
seen = undefined;
(1)[key] = 1;
assert.sameValue(seen, "number");
