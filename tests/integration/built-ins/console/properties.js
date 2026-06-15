/*---
description: Supported properties and attributes of the console object.
includes: [propertyHelper.js]
---*/

// Console object is a property of the global object
assert.sameValue(typeof console, "object");
assert.notSameValue(console, null);

verifyProperty(this, "console", {
  writable: true,
  enumerable: false,
  configurable: true,
}, {
  restore: true,
});

// Verify attributes of console methods
const methods = ["debug", "error", "info", "log", "warn"];

for (let i = 0; i < methods.length; i++) {
  const name = methods[i];
  const method = console[name];

  assert.sameValue(typeof method, "function");

  verifyProperty(method, "name", {
    value: name,
    writable: false,
    enumerable: false,
    configurable: true,
  });

  verifyProperty(method, "length", {
    value: 0,
    writable: false,
    enumerable: false,
    configurable: true,
  });

  verifyProperty(console, name, {
    writable: true,
    enumerable: false,
    configurable: true,
  });
}
