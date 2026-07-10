/*---
description: >
  Unicode code point escape sequences `\u{...}` of any length are parsed correctly, including with
  any number of leading zeros.
---*/

// In string literals
assert.sameValue("\u{0000000000000061}", "a");
assert.sameValue("\u{0000000000}".charCodeAt(0), 0);
assert.sameValue("\u{00000010FFFF}", "\u{10FFFF}");

// In template literals
assert.sameValue(`\u{0000000000000061}`, "a");
assert.sameValue(`\u{0000000000}`, "\0");
assert.sameValue(`\u{00000010FFFF}`, "\u{10FFFF}");

// In identifiers
const \u{0000000061} = 1;
assert.sameValue(a, 1);
var b\u{0000000063} = 2;
assert.sameValue(bc, 2);

// In private identifiers
class C {
  #\u{0000000064} = 5;

  get() {
    return this.#d;
  }
}
assert.sameValue(new C().get(), 5);

// Throws a SyntaxError if out of range
assert.throws(SyntaxError, function () {
  eval('"\\u{00000110000}"');
});
assert.throws(SyntaxError, function () {
  eval('"\\u{0000000FFFFFFFF}"');
});
