/*---
description: Caching of named property accesses on primitive receivers.
---*/

// A single cache entry serves every string, whatever its representation
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.myProperty;
    }
    return last;
  }

  String.prototype.myProperty = "found";

  var strings = [
    "abcdefgh",
    "abcd" + "efgh",
    ("ab" + "cd") + ("ef" + "gh"),
    "xxabcdefghxx".slice(2, 10),
    "中文",
    "\u{1f600}",
    "",
  ];

  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < strings.length; i++) {
      assert.sameValue(read(strings[i]), "found");
    }
  }
})();

// The property is read live from the prototype, so overwriting it needs no invalidation
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.myProperty;
    }
    return last;
  }

  String.prototype.myProperty = "first";
  assert.sameValue(read("abc"), "first");

  String.prototype.myProperty = "second";
  assert.sameValue(read("abc"), "second");
})();

// Methods are found and called with the primitive as the receiver
(function () {
  function charAt(s) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = s.charCodeAt(0);
    }
    return last;
  }

  assert.sameValue(charAt("abc"), 97);
  assert.sameValue(charAt("b" + "cd"), 98);
  assert.sameValue("abcd".slice(1), "bcd");

  // Whether `this` is the primitive or a wrapper depends on the strictness of the method, so only
  // a method with its own directive is checked here.
  String.prototype.strictThis = function () { "use strict"; return typeof this; };
  String.prototype.thisAsString = function () { return String(this); };

  for (var i = 0; i < 100; i++) {
    assert.sameValue("abc".strictThis(), "string");
    assert.sameValue("abc".thisAsString(), "abc");
  }
})();

// Symbols and bigints are cached on their own kinds and share a callsite with strings
(function () {
  function read(o) {
    return o.myProperty;
  }

  var sym = Symbol("s");

  String.prototype.myProperty = "string";
  Symbol.prototype.myProperty = "symbol";
  BigInt.prototype.myProperty = "bigint";

  for (var round = 0; round < 100; round++) {
    assert.sameValue(read("abc"), "string");
    assert.sameValue(read(sym), "symbol");
    assert.sameValue(read(10n), "bigint");
  }

  // Each entry reads from its own holder
  Symbol.prototype.myProperty = "symbol2";
  assert.sameValue(read(sym), "symbol2");
  assert.sameValue(read("abc"), "string");
  assert.sameValue(read(10n), "bigint");
})();

// A string's length is served by its own cache and is unaffected by the prototype chain
(function () {
  function len(s) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = s.length;
    }
    return last;
  }

  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.myProperty;
    }
    return last;
  }

  String.prototype.myProperty = "found";
  assert.sameValue(len("abcde"), 5);
  assert.sameValue(read("abcde"), "found");
  assert.sameValue(len("ab"), 2);
})();

// Wrapper objects are a different receiver from primitives and stay correct alongside them
(function () {
  function read(o) {
    return o.myProperty;
  }

  String.prototype.myProperty = "shared";

  var wrapper = new String("ab");
  for (var round = 0; round < 100; round++) {
    assert.sameValue(read("abc"), "shared");
    assert.sameValue(read(wrapper), "shared");
  }

  // An own property on the wrapper shadows the prototype for that receiver only
  wrapper.myProperty = "own";
  assert.sameValue(read(wrapper), "own");
  assert.sameValue(read("abc"), "shared");
})();

// Receivers with no prototype chain to reach behave normally at a warm callsite
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.myProperty;
    }
    return last;
  }

  String.prototype.myProperty = "found";
  assert.sameValue(read("abc"), "found");

  assert.throws(TypeError, function () { read(null); });
  assert.throws(TypeError, function () { read(undefined); });
  assert.sameValue(read({}), undefined);
  assert.sameValue(read("abc"), "found");
})();

// Numbers and booleans have no shape so they are never cached, but must still read correctly
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 10; i++) {
      last = o.myProperty;
    }
    return last;
  }

  Number.prototype.myProperty = "number";
  Boolean.prototype.myProperty = "boolean";

  for (var round = 0; round < 10; round++) {
    assert.sameValue(read(5), "number");
    assert.sameValue(read(1.5), "number");
    assert.sameValue(read(true), "boolean");
  }

  Number.prototype.myProperty = "number2";
  assert.sameValue(read(5), "number2");

  assert.sameValue((5).toFixed(2), "5.00");
  assert.sameValue(true.toString(), "true");
})();

// The cache survives garbage collection
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.myProperty;
    }
    return last;
  }

  String.prototype.myProperty = "found";

  assert.sameValue(read("abc"), "found");
  $262.gc();
  assert.sameValue(read("abc"), "found");

  $262.gc();
  assert.sameValue(read("de" + "fg"), "found");
})();

// A primitive from another realm resolves through the prototype of the realm that is running, so a
// shared callsite stays on a single entry
(function () {
  function read(o) {
    return o.myProperty;
  }

  var other = $262.createRealm();
  other.evalScript("globalThis.make = function () { return 'abcdefg'; };");
  other.evalScript("String.prototype.myProperty = 'theirs';");
  var theirs = other.global.make();

  String.prototype.myProperty = "mine";

  for (var i = 0; i < 100; i++) {
    assert.sameValue(read("abc"), "mine");
    assert.sameValue(read(theirs), "mine");
  }
})();

// Accessors are never cached for primitive receivers, but must still run on every read
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.accessorProperty;
    }
    return last;
  }

  var calls = 0;
  Object.defineProperty(String.prototype, "accessorProperty", {
    get: function () { calls++; return String(this); },
    configurable: true,
  });

  assert.sameValue(read("abc"), "abc");
  assert.sameValue(calls, 100);
})();

// Keys that may be canonical numeric index strings are intercepted by the string itself and are
// never cached on the prototype
(function () {
  function read(o, key) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o[key];
    }
    return last;
  }

  String.prototype["0"] = "proto zero";
  String.prototype["1.5"] = "proto fraction";

  // An in range index comes from the string, not the prototype
  assert.sameValue(read("abc", "0"), "a");
  // An out of range index falls through to the prototype
  assert.sameValue(read("", "0"), "proto zero");
  // A numeric string that is not an index never comes from the string
  assert.sameValue(read("abc", "1.5"), "proto fraction");
})();

// A wrapper object returned by ToObject has exactly the shape a primitive coerces to, while an
// entry for a primitive receiver is keyed on the primitive's own shape. A callsite that sees the
// wrapper first must still promote to a polymorphic cache once it sees the primitive.
(function () {
  // Each kind reads at its own callsite so that each one fills its own cache
  function readString(o) { return o.wrapperFirst; }
  function readAbsent(o) { return o.wrapperFirstAbsent; }
  function readSymbol(o) { return o.wrapperFirst; }
  function readBigInt(o) { return o.wrapperFirst; }

  String.prototype.wrapperFirst = "string";
  Symbol.prototype.wrapperFirst = "symbol";
  BigInt.prototype.wrapperFirst = "bigint";

  var sym = Symbol("s");

  // Note that Object() returns a wrapper with the shape that ToObject produces, while
  // new String() does not, so only the former shares a shape with a coerced primitive.
  assert.sameValue(readString(Object("abc")), "string");
  assert.sameValue(readString("abc"), "string");
  assert.sameValue(readString(Object("abc")), "string");
  assert.sameValue(readString("abc"), "string");

  // An absent property is cached as well, and is keyed the same way
  assert.sameValue(readAbsent(Object("abc")), undefined);
  assert.sameValue(readAbsent("abc"), undefined);

  assert.sameValue(readSymbol(Object(sym)), "symbol");
  assert.sameValue(readSymbol(sym), "symbol");

  assert.sameValue(readBigInt(Object(10n)), "bigint");
  assert.sameValue(readBigInt(10n), "bigint");
})();

// Everything below deletes from String.prototype, which makes later accesses uncacheable.

// An absent property is cached too, and is found once it appears on the prototype chain
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.absentProperty;
    }
    return last;
  }

  assert.sameValue(read("abc"), undefined);
  assert.sameValue(read("de"), undefined);

  String.prototype.absentProperty = "appeared";
  assert.sameValue(read("abc"), "appeared");

  delete String.prototype.absentProperty;
  assert.sameValue(read("abc"), undefined);
})();

// A property further up the prototype chain is found through String.prototype, and a closer
// property added later shadows it
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.shadowedProperty;
    }
    return last;
  }

  Object.prototype.shadowedProperty = "from Object.prototype";
  assert.sameValue(read("abc"), "from Object.prototype");

  String.prototype.shadowedProperty = "from String.prototype";
  assert.sameValue(read("abc"), "from String.prototype");

  delete String.prototype.shadowedProperty;
  assert.sameValue(read("abc"), "from Object.prototype");

  delete Object.prototype.shadowedProperty;
  assert.sameValue(read("abc"), undefined);
})();

// Replacing the prototype of String.prototype invalidates entries that reach through it
(function () {
  function read(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.movedProperty;
    }
    return last;
  }

  String.prototype.movedProperty = "on String.prototype";
  assert.sameValue(read("abc"), "on String.prototype");

  Object.setPrototypeOf(String.prototype, { movedProperty: "on a new grandparent" });
  delete String.prototype.movedProperty;
  assert.sameValue(read("abc"), "on a new grandparent");

  Object.setPrototypeOf(String.prototype, Object.prototype);
  assert.sameValue(read("abc"), undefined);
})();
