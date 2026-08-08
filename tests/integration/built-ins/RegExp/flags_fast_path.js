/*---
description: >
  RegExp methods read the flags off the RegExp directly instead of calling the `flags` getter where
  possible. Verify that this gives the same results as calling the getter, and that shadowing,
  replacing, or deleting any of the getters it reads is still observed.
includes: [compareArray.js]
---*/

var singleFlags = [
  ["d", "hasIndices"],
  ["g", "global"],
  ["i", "ignoreCase"],
  ["m", "multiline"],
  ["s", "dotAll"],
  ["u", "unicode"],
  ["v", "unicodeSets"],
  ["y", "sticky"],
];

// Every operation that reads the `flags` property, applied to a freshly created RegExp so that no
// last index state is carried between them. String.prototype.matchAll requires a global RegExp.
function runAll(makeRegExp, string) {
  return {
    flags: makeRegExp().flags,
    match: JSON.stringify(string.match(makeRegExp())),
    matchAll: makeRegExp().global ? JSON.stringify([...string.matchAll(makeRegExp())]) : "",
    replace: string.replace(makeRegExp(), "<$&|$1>"),
    split: string.split(makeRegExp()),
  };
}

// Two receivers whose flags are observably identical but which cannot be read directly: one has an
// extra own property, and one is a subclass instance whose prototype is not the RegExp prototype.
// Both still inherit every flag getter from the RegExp prototype.
function assertSameResults(source, flags, string) {
  var Subclass = class extends RegExp {};
  var message = "/" + source + "/" + flags + " on " + JSON.stringify(string);

  var direct = runAll(function () { return new RegExp(source, flags); }, string);

  [
    ["own property", function () {
      var regexp = new RegExp(source, flags);
      regexp.marker = 1;
      return regexp;
    }],
    ["subclass", function () { return new Subclass(source, flags); }],
  ].forEach(function (receiver) {
    var other = runAll(receiver[1], string);
    var suffix = " (" + receiver[0] + ")";

    assert.sameValue(direct.flags, other.flags, message + " flags" + suffix);
    assert.sameValue(direct.match, other.match, message + " match" + suffix);
    assert.sameValue(direct.matchAll, other.matchAll, message + " matchAll" + suffix);
    assert.sameValue(direct.replace, other.replace, message + " replace" + suffix);
    assert.compareArray(direct.split, other.split, message + " split" + suffix);
  });
}

var sources = ["a", "(a)b", "\u{1F600}"];
var flagCombinations = ["", "g", "gi", "gy", "gu", "dgimsy"];
var strings = ["", "aXbXc", "a1b2c", "\u{1F600}a\u{1F600}"];

for (var i = 0; i < sources.length; i++) {
  for (var j = 0; j < flagCombinations.length; j++) {
    for (var k = 0; k < strings.length; k++) {
      assertSameResults(sources[i], flagCombinations[j], strings[k]);
    }
  }
}

// The flags string is exactly what the individual getters report, in `dgimsuvy` order.
for (var bits = 0; bits < 1 << singleFlags.length; bits++) {
  var flagsString = "";
  for (var i = 0; i < singleFlags.length; i++) {
    if (bits & (1 << i)) {
      flagsString += singleFlags[i][0];
    }
  }

  // The `u` and `v` flags are mutually exclusive
  if (flagsString.indexOf("u") !== -1 && flagsString.indexOf("v") !== -1) {
    continue;
  }

  var regexp = new RegExp("a", flagsString);
  var message = " for " + JSON.stringify(flagsString);

  assert.sameValue(regexp.flags, flagsString, "flags" + message);
  for (var i = 0; i < singleFlags.length; i++) {
    assert.sameValue(
      regexp[singleFlags[i][1]],
      (bits & (1 << i)) !== 0,
      singleFlags[i][1] + message
    );
  }
}

// Writing the last index does not add a property, so the flags are still read directly.
var withLastIndex = new RegExp("a", "gi");
withLastIndex.lastIndex = 7;
assert.sameValue(withLastIndex.flags, "gi", "flags after a last index write");

// An own property shadows the prototype getter that `flags` reads.
var ownGlobal = new RegExp("a", "");
Object.defineProperty(ownGlobal, "global", { value: true });
assert.sameValue(ownGlobal.flags, "g", "own `global` property is read by the `flags` getter");

var ownFlags = new RegExp("a", "g");
Object.defineProperty(ownFlags, "flags", { value: "" });
assert.sameValue("aa".replace(ownFlags, "-"), "-a", "own `flags` property is read by @@replace");

// A subclass getter shadows it as well.
class NotGlobal extends RegExp {
  get global() {
    return false;
  }
}

assert.sameValue(new NotGlobal("a", "g").flags, "", "subclass getter is read by the `flags` getter");
assert.sameValue(
  "aa".replace(new NotGlobal("a", "g"), "-"),
  "-a",
  "subclass getter is read by @@replace"
);

// Replace a getter on the RegExp prototype for the duration of a callback.
function withGetter(name, value, callback) {
  var original = Object.getOwnPropertyDescriptor(RegExp.prototype, name);
  Object.defineProperty(RegExp.prototype, name, {
    get: function () { return value; },
    configurable: true,
  });

  try {
    return callback();
  } finally {
    Object.defineProperty(RegExp.prototype, name, original);
  }
}

// Each replaced flag getter is read by the `flags` getter.
for (var i = 0; i < singleFlags.length; i++) {
  var getterName = singleFlags[i][1];

  assert.sameValue(
    withGetter(getterName, true, function () { return new RegExp("a", "").flags; }),
    singleFlags[i][0],
    "replaced `" + getterName + "` getter is read by the `flags` getter"
  );
}

// A replaced `flags` or `global` getter is read by the methods built on it.
assert.sameValue(
  withGetter("flags", "", function () { return "aa".replace(new RegExp("a", "g"), "-"); }),
  "-a",
  "replaced `flags` getter is read by @@replace"
);
assert.compareArray(
  withGetter("global", false, function () { return "aa".match(new RegExp("a", "g")); }),
  ["a"],
  "replaced `global` getter is read by @@match"
);

// A `global` of true on a non-global RegExp that matches never terminates, since the builtin `exec`
// does not advance the last index, so observe it through the last index reset instead.
var resetsLastIndex = new RegExp("a", "");
resetsLastIndex.lastIndex = 5;
withGetter("global", true, function () { return "bbb".replace(resetsLastIndex, "-"); });
assert.sameValue(resetsLastIndex.lastIndex, 0, "replaced `global` getter makes @@replace reset it");

// Deleting a getter is observed, and restoring it takes effect again.
var originalGlobal = Object.getOwnPropertyDescriptor(RegExp.prototype, "global");
delete RegExp.prototype.global;
assert.sameValue(new RegExp("a", "g").flags, "", "flags after `global` is deleted");
Object.defineProperty(RegExp.prototype, "global", originalGlobal);
assert.sameValue(new RegExp("a", "g").flags, "g", "flags after `global` is restored");
assert.sameValue("aa".replace(new RegExp("a", "g"), "-"), "--", "@@replace after it is restored");

// A change to the RegExp prototype that leaves the getters alone must not change any results.
RegExp.prototype.unrelatedProperty = 1;
assert.sameValue(new RegExp("a", "gi").flags, "gi", "flags after an unrelated property is added");
assert.sameValue(
  "aa".replace(new RegExp("a", "g"), "-"),
  "--",
  "@@replace after an unrelated property is added"
);
delete RegExp.prototype.unrelatedProperty;
assert.sameValue(new RegExp("a", "gi").flags, "gi", "flags after an unrelated property is removed");

// @@split and @@matchAll hand the flags string to their species constructor, with @@split adding
// the sticky flag if it is not already present. The flags string must be the same whichever path
// produced it, so run every case against both.
//
// Reaching the species constructor through an own `constructor` property adds a property to the
// receiver, which disables the fast path and reads the flags from the getter. Installing @@species
// on the RegExp constructor instead leaves the receiver untouched, so the flags are read directly
// off the RegExp.
function speciesFlagsFromGetter(regexp, callback) {
  var seenFlags = null;

  regexp.constructor = {};
  regexp.constructor[Symbol.species] = function (source, flags) {
    seenFlags = flags;
    return new RegExp(source.source, flags);
  };

  callback(regexp);

  return seenFlags;
}

function speciesFlagsDirect(regexp, callback) {
  var seenFlags = null;
  var original = Object.getOwnPropertyDescriptor(RegExp, Symbol.species);

  Object.defineProperty(RegExp, Symbol.species, {
    value: function (source, flags) {
      seenFlags = flags;
      return new RegExp(source.source, flags);
    },
    configurable: true,
  });

  try {
    callback(regexp);
  } finally {
    Object.defineProperty(RegExp, Symbol.species, original);
  }

  return seenFlags;
}

function splitWith(regexp) {
  "aXb".split(regexp);
}

function matchAllWith(regexp) {
  [...regexp[Symbol.matchAll]("a")];
}

[["from the getter", speciesFlagsFromGetter], ["read directly", speciesFlagsDirect]].forEach(
  function (variant) {
    var speciesFlags = variant[1];
    var suffix = " (" + variant[0] + ")";

    assert.sameValue(
      speciesFlags(new RegExp("X", ""), splitWith),
      "y",
      "@@split adds the sticky flag" + suffix
    );
    assert.sameValue(
      speciesFlags(new RegExp("X", "gi"), splitWith),
      "giy",
      "@@split appends it last" + suffix
    );
    assert.sameValue(
      speciesFlags(new RegExp("X", "gy"), splitWith),
      "gy",
      "@@split keeps it once" + suffix
    );
    assert.sameValue(
      withGetter("flags", "im", function () {
        return speciesFlags(new RegExp("X", ""), splitWith);
      }),
      "imy",
      "@@split adds the sticky flag to a replaced `flags` getter" + suffix
    );
    assert.sameValue(
      speciesFlags(new RegExp("a", "gim"), matchAllWith),
      "gim",
      "@@matchAll passes the flags string through" + suffix
    );
  }
);
