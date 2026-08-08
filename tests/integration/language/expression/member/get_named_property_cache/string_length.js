/*---
description: String primitive length reads are cached, only checking that the receiver is a string.
---*/

// Each site is read in a loop so the inline cache is filled and then re-matched.
function readLength(s) {
  var last;
  for (var i = 0; i < 100; i++) {
    last = s.length;
  }
  return last;
}

// A single cache entry must serve every string, not just the first one seen
assert.sameValue(readLength(""), 0);
assert.sameValue(readLength("a"), 1);
assert.sameValue(readLength("abcdef"), 6);

// Every string representation is matched by the same entry: flat strings, ropes built by
// concatenation, substrings, and strings produced by builtins
(function () {
  var flat = "abcdefgh";
  var rope = "abcd" + "efgh";
  var nestedRope = ("ab" + "cd") + ("ef" + "gh");
  var sliced = "xxabcdefghxx".slice(2, 10);
  var repeated = "abcd".repeat(2);
  var fromCharCode = String.fromCharCode(97, 98, 99, 100, 101, 102, 103, 104);
  var converted = String(12345678);
  var templated = `abcd${"efgh"}`;

  var strings = [flat, rope, nestedRope, sliced, repeated, fromCharCode, converted, templated];
  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < strings.length; i++) {
      assert.sameValue(readLength(strings[i]), 8);
    }
  }
})();

// Length is the number of UTF-16 code units, not code points or bytes
(function () {
  assert.sameValue(readLength("éé"), 2);
  assert.sameValue(readLength("中文"), 2);

  // Astral characters are a surrogate pair, so they count as two code units
  assert.sameValue(readLength("\u{1f600}"), 2);
  assert.sameValue(readLength("a\u{1f600}b"), 4);

  // A lone surrogate is a single code unit
  assert.sameValue(readLength("\ud83d"), 1);

  // A two byte rope keeps its length without being flattened
  assert.sameValue(readLength("中" + "文" + "abc"), 5);
})();

// Strings are immutable, so a cached length can never go stale for a given receiver. Reassigning
// the variable produces a different string, which the same entry must still match.
(function () {
  var s = "abc";
  assert.sameValue(readLength(s), 3);
  s += "de";
  assert.sameValue(readLength(s), 5);
  s = s.slice(1);
  assert.sameValue(readLength(s), 4);
})();

// String.prototype has its own non-configurable length, so a length accessor can never be installed
// anywhere a string primitive would reach it.
(function () {
  assert.sameValue(readLength("abcde"), 5);

  var descriptor = Object.getOwnPropertyDescriptor(String.prototype, "length");
  assert.sameValue(descriptor.configurable, false);
  assert.sameValue(descriptor.writable, false);
  assert.sameValue(descriptor.value, 0);

  assert.throws(TypeError, function () {
    Object.defineProperty(String.prototype, "length", { get: function () { return 99; } });
  });
  assert.sameValue(readLength("abcde"), 5);

  assert.throws(TypeError, function () {
    "use strict";
    delete String.prototype.length;
  });
  assert.sameValue(readLength("abcde"), 5);
})();

// A length further up the prototype chain is shadowed by the own length of the object that ToObject
// creates, so it must never be observed.
(function () {
  assert.sameValue(readLength("abcde"), 5);

  Object.prototype.length = 99;
  assert.sameValue(readLength("abcde"), 5);
  assert.sameValue("ab".length, 2);
  delete Object.prototype.length;

  Object.defineProperty(Object.prototype, "length", {
    get: function () { return 99; },
    configurable: true,
  });
  assert.sameValue(readLength("abcde"), 5);
  delete Object.prototype.length;

  assert.sameValue(readLength("abcde"), 5);
})();

// Reading length off a primitive never leaks a wrapper object as the receiver
(function () {
  function readInStrict(s) {
    "use strict";
    var last;
    for (var i = 0; i < 100; i++) {
      last = s.length;
    }
    return last;
  }

  assert.sameValue(readInStrict("abcd"), 4);
  assert.sameValue(readLength("abcd"), 4);
})();

// A String wrapper object is a different receiver from a string primitive. Its length is an
// ordinary own property, and both must stay correct at a shared callsite.
(function () {
  function len(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.length;
    }
    return last;
  }

  assert.sameValue(len("abc"), 3);
  assert.sameValue(len(new String("abcd")), 4);
  assert.sameValue(len("abc"), 3);
  assert.sameValue(len(String.prototype), 0);

  // String subclass instances are wrapper objects too
  class Sub extends String {}
  assert.sameValue(len(new Sub("abcde")), 5);
  assert.sameValue(len("abc"), 3);
})();

// Strings and arrays are both matched on the receiver's kind rather than a shape. They must not
// replace each other in a monomorphic cache, in either fill order.
(function () {
  function len(o) {
    return o.length;
  }

  var arr = [1, 2, 3];
  for (var i = 0; i < 100; i++) {
    assert.sameValue(len("abcdefg"), 7);
    assert.sameValue(len(arr), 3);
  }

  // Both entries stay live as their receivers change
  arr.push(4);
  assert.sameValue(len(arr), 4);
  assert.sameValue(len("ab"), 2);
  assert.sameValue(len(arr), 4);
})();

(function () {
  function len(o) {
    return o.length;
  }

  // Fill the array entry first this time
  var arr = [1, 2];
  for (var i = 0; i < 100; i++) {
    assert.sameValue(len(arr), 2);
    assert.sameValue(len("abc"), 3);
  }

  arr.length = 0;
  assert.sameValue(len(arr), 0);
  assert.sameValue(len("abcd"), 4);
})();

// A callsite that sees strings, arrays, plain objects and a wrapper holds all four kinds at once
(function () {
  function len(o) {
    return o.length;
  }

  var arr = [1, 2, 3];
  var plain = { length: 7 };
  var wrapper = new String("ab");

  for (var round = 0; round < 100; round++) {
    assert.sameValue(len("abcde"), 5);
    assert.sameValue(len(arr), 3);
    assert.sameValue(len(plain), 7);
    assert.sameValue(len(wrapper), 2);
  }

  arr.push(4);
  plain.length = 8;
  assert.sameValue(len("abcde"), 5);
  assert.sameValue(len(arr), 4);
  assert.sameValue(len(plain), 8);
})();

// A string fills a polymorphic slot like any other entry. A fifth receiver overflows the cache and
// caching stops, but every receiver still reads correctly, including the string.
(function () {
  function len(o) {
    return o.length;
  }

  var arr = [1, 2];
  var one = { length: 3 };
  var two = { pad: 0, length: 4 };
  var fifth = { pad: 0, pad2: 0, length: 5 };

  // Fill and hit all four entries before introducing a fifth receiver
  for (var round = 0; round < 2; round++) {
    assert.sameValue(len("a"), 1);
    assert.sameValue(len(arr), 2);
    assert.sameValue(len(one), 3);
    assert.sameValue(len(two), 4);
  }

  // The fifth shape does not fit, so caching stops at this callsite
  assert.sameValue(len(fifth), 5);

  for (var round2 = 0; round2 < 2; round2++) {
    assert.sameValue(len("a"), 1);
    assert.sameValue(len(arr), 2);
    assert.sameValue(len(one), 3);
    assert.sameValue(len(two), 4);
    assert.sameValue(len(fifth), 5);
  }

  // Receivers are still read live once caching has stopped
  arr.push(0);
  one.length = 9;
  assert.sameValue(len("abcd"), 4);
  assert.sameValue(len(arr), 3);
  assert.sameValue(len(one), 9);
})();

// Named properties other than length are not cached for string primitives, and reading them must
// not disturb a length site or return a wrong value.
(function () {
  function get(o, key) {
    return o[key];
  }

  assert.sameValue(readLength("abcd"), 4);
  assert.sameValue("abcd".charCodeAt(0), 97);
  assert.sameValue("abcd".slice(1), "bcd");
  assert.sameValue("abcd".nonexistent, undefined);
  assert.sameValue(readLength("abcd"), 4);

  // Computed access reaches the same value through a different opcode
  for (var i = 0; i < 100; i++) {
    assert.sameValue(get("abcde", "length"), 5);
  }
  assert.sameValue(get("abcde", "0"), "a");
  assert.sameValue(get("abcde", 0), "a");
})();

// Methods added to String.prototype after a length site is warm are still found
(function () {
  assert.sameValue(readLength("abcd"), 4);

  String.prototype.myLength = function () { return this.length; };
  assert.sameValue("abcde".myLength(), 5);
  assert.sameValue(readLength("abcd"), 4);
  delete String.prototype.myLength;
})();

// Values that are not strings at a warm string length site behave normally
(function () {
  function len(o) {
    return o.length;
  }

  for (var i = 0; i < 100; i++) {
    assert.sameValue(len("abc"), 3);
  }

  assert.throws(TypeError, function () { len(null); });
  assert.throws(TypeError, function () { len(undefined); });
  assert.sameValue(len(5), undefined);
  assert.sameValue(len(true), undefined);
  assert.sameValue(len(Symbol("x")), undefined);
  assert.sameValue(len(10n), undefined);
  assert.sameValue(len(function (a, b) {}), 2);

  assert.sameValue(len("abcd"), 4);
})();

// A proxy that reports a length is not a string and must still route through its trap
(function () {
  function len(o) {
    return o.length;
  }

  assert.sameValue(len("abc"), 3);

  var trapped = 0;
  var proxy = new Proxy({ length: 4 }, {
    get: function (target, key, receiver) {
      if (key === "length") {
        trapped++;
      }
      return Reflect.get(target, key, receiver);
    },
  });

  for (var i = 0; i < 10; i++) {
    assert.sameValue(len(proxy), 4);
    assert.sameValue(len("abc"), 3);
  }
  assert.sameValue(trapped, 10);
})();

// The cache survives garbage collection
(function () {
  function len(o) {
    return o.length;
  }

  var arr = [1, 2, 3];
  assert.sameValue(len("abcd"), 4);
  assert.sameValue(len(arr), 3);

  $262.gc();

  assert.sameValue(len("abcd"), 4);
  assert.sameValue(len(arr), 3);

  // A rope allocated after the collection is matched by the same entry
  $262.gc();
  assert.sameValue(len("ab" + "cde"), 5);
})();

// Strings from another realm are ordinary strings, so a shared callsite stays monomorphic
(function () {
  var other = $262.createRealm();
  other.evalScript("globalThis.make = function () { return 'abcdefg'; };");

  function len(o) {
    return o.length;
  }

  var theirs = other.global.make();
  for (var i = 0; i < 100; i++) {
    assert.sameValue(len("abc"), 3);
    assert.sameValue(len(theirs), 7);
  }
})();
