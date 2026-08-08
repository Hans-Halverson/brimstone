/*---
description: >
  String.prototype.matchAll and String.prototype.replaceAll decide whether their argument is global
  from the string returned by the `flags` getter, not from the flags stored on the RegExp. Verify
  that replacing `flags`, replacing a getter that it reads, or shadowing either one on the RegExp
  itself is observed by both methods, including once the fast path for reading flags directly off a
  RegExp has already been taken and cached, and that the cache is kept separate between realms.
includes: [compareArray.js]
---*/

// No pattern below matches `subject`. A RegExp that only reports itself as global would otherwise
// loop forever in @@replace, since the builtin `exec` does not advance the last index of a RegExp
// that is not really global. matchAll is checked against real matches in `matchSubject` instead,
// which is safe because @@matchAll iterates a species copy that really is global.
var subject = "zzz";
var matchSubject = "aaa";

function assertAccepted(makeRegExp, expectedMatches, message) {
  assert.compareArray(
    [...matchSubject.matchAll(makeRegExp())].map(function (match) { return match[0]; }),
    expectedMatches,
    "String.prototype.matchAll " + message
  );
  assert.sameValue(
    subject.replaceAll(makeRegExp(), "-"),
    subject,
    "String.prototype.replaceAll " + message
  );
}

function assertRejected(makeRegExp, message) {
  assert.throws(
    TypeError,
    function () { [...subject.matchAll(makeRegExp())]; },
    "String.prototype.matchAll " + message
  );
  assert.throws(
    TypeError,
    function () { subject.replaceAll(makeRegExp(), "-"); },
    "String.prototype.replaceAll " + message
  );
}

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

// Ordinary use, which is what takes the fast path. This runs before anything below mutates
// RegExp.prototype: a case that only ever runs against an uninitialized cache cannot tell a correct
// recompute apart from a stale cached answer.
function assertOrdinaryUseUnaffected(label) {
  assert.compareArray(
    [..."aXbX".matchAll(/X/g)].map(function (match) { return match.index; }),
    [1, 3],
    "matchAll on an ordinary global RegExp " + label
  );
  assert.sameValue(
    "aXbX".replaceAll(/X/g, "-"),
    "a-b-",
    "replaceAll on an ordinary global RegExp " + label
  );
  assert.sameValue(
    "aXbX".replaceAll("X", "-"),
    "a-b-",
    "replaceAll with a string search value " + label
  );
  assert.sameValue(/a/gi.flags, "gi", "flags getter " + label);
  assert.sameValue(/a/.flags, "", "flags getter with no flags " + label);

  assert.throws(
    TypeError,
    function () { [..."aXbX".matchAll(/X/)]; },
    "matchAll rejects an ordinary non-global RegExp " + label
  );
  assert.throws(
    TypeError,
    function () { "aXbX".replaceAll(/X/, "-"); },
    "replaceAll rejects an ordinary non-global RegExp " + label
  );
}

assertOrdinaryUseUnaffected("(warm-up)");

// A RegExp that is not global is still accepted when the flags string reports that it is.
withGetter("global", true, function () {
  assertAccepted(
    function () { return new RegExp("a", ""); },
    ["a", "a", "a"],
    "accepts a non-global RegExp whose replaced `global` getter reports true"
  );
});

withGetter("flags", "g", function () {
  assertAccepted(
    function () { return new RegExp("a", ""); },
    ["a", "a", "a"],
    "accepts a non-global RegExp whose replaced `flags` getter reports \"g\""
  );
});

assertAccepted(function () {
  var regexp = new RegExp("a", "");
  Object.defineProperty(regexp, "global", { value: true });
  return regexp;
}, ["a", "a", "a"], "accepts a non-global RegExp with an own `global` property of true");

assertAccepted(function () {
  var regexp = new RegExp("a", "");
  Object.defineProperty(regexp, "flags", { value: "g" });
  return regexp;
}, ["a", "a", "a"], "accepts a non-global RegExp with an own `flags` property of \"g\"");

// A global RegExp is rejected when the flags string reports that it is not.
withGetter("global", false, function () {
  assertRejected(
    function () { return new RegExp("a", "g"); },
    "rejects a global RegExp whose replaced `global` getter reports false"
  );
});

withGetter("flags", "", function () {
  assertRejected(
    function () { return new RegExp("a", "g"); },
    "rejects a global RegExp whose replaced `flags` getter reports \"\""
  );
});

assertRejected(function () {
  var regexp = new RegExp("a", "g");
  Object.defineProperty(regexp, "global", { value: false });
  return regexp;
}, "rejects a global RegExp with an own `global` property of false");

assertRejected(function () {
  var regexp = new RegExp("a", "g");
  Object.defineProperty(regexp, "flags", { value: "" });
  return regexp;
}, "rejects a global RegExp with an own `flags` property of \"\"");

// The argument does not have to be a RegExp at all. IsRegExp is true for any object with a truthy
// @@match, and the check reads the `flags` property of whatever it is given. Such an object has no
// @@matchAll or @@replace, so both methods fall back to using its string form as the pattern, which
// matches neither subject.
function matchableObject(flags) {
  var object = {};
  object[Symbol.match] = true;
  object.flags = flags;
  return object;
}

assertAccepted(
  function () { return matchableObject("g"); },
  [],
  "accepts a non-RegExp object whose `flags` property contains \"g\""
);
assertRejected(
  function () { return matchableObject(""); },
  "rejects a non-RegExp object whose `flags` property does not contain \"g\""
);

// The flags value is checked with RequireObjectCoercible before it is converted to a string.
assertRejected(
  function () { return matchableObject(undefined); },
  "rejects a `flags` value of undefined"
);
assertRejected(
  function () { return matchableObject(null); },
  "rejects a `flags` value of null"
);

// Repeat the warm/invalidate/restore cycle. A cache that went stale in either direction - kept
// reporting the builtin getters after they were replaced, or stayed disabled after they were put
// back - shows up on a later round rather than the first.
for (var round = 1; round <= 2; round++) {
  var label = "(round " + round + ")";

  assertOrdinaryUseUnaffected(label);

  withGetter("global", true, function () {
    assertAccepted(
      function () { return new RegExp("a", ""); },
      ["a", "a", "a"],
      "accepts a non-global RegExp whose replaced `global` getter reports true " + label
    );
  });

  withGetter("global", false, function () {
    assertRejected(
      function () { return new RegExp("a", "g"); },
      "rejects a global RegExp whose replaced `global` getter reports false " + label
    );
  });

  assertOrdinaryUseUnaffected(label + " after restore");
}

// It is the identity of each getter that matters, not merely that it is some builtin, so binding a
// different builtin flag getter under the `global` name must disable the fast path.
(function () {
  var stickyGetter = Object.getOwnPropertyDescriptor(RegExp.prototype, "sticky").get;
  var original = Object.getOwnPropertyDescriptor(RegExp.prototype, "global");

  Object.defineProperty(RegExp.prototype, "global", { get: stickyGetter, configurable: true });

  try {
    assert.sameValue(
      new RegExp("a", "y").flags,
      "gy",
      "flags getter observes `global` bound to the sticky getter"
    );
    assertAccepted(
      function () { return new RegExp("a", "y"); },
      ["a", "a", "a"],
      "accepts a sticky RegExp once `global` is bound to the sticky getter"
    );
    assertRejected(
      function () { return new RegExp("a", "g"); },
      "rejects a global but non-sticky RegExp once `global` is bound to the sticky getter"
    );
  } finally {
    Object.defineProperty(RegExp.prototype, "global", original);
  }

  assertOrdinaryUseUnaffected("(after restoring global)");
})();

// RegExp.prototype[@@matchAll] reads `flags` itself and hands the result to the species
// constructor, so a replaced getter has to change the flags of the matcher it builds.
(function () {
  var seenFlags = null;

  function Species(regexp, flags) {
    seenFlags = flags;
    return new RegExp(regexp, flags);
  }

  function matchAllFlagsFor(regexp) {
    var original = Object.getOwnPropertyDescriptor(RegExp, Symbol.species);
    Object.defineProperty(RegExp, Symbol.species, { value: Species, configurable: true });
    seenFlags = null;

    try {
      [...regexp[Symbol.matchAll](matchSubject)];
    } finally {
      Object.defineProperty(RegExp, Symbol.species, original);
    }

    return seenFlags;
  }

  assert.sameValue(
    matchAllFlagsFor(/a/g),
    "g",
    "@@matchAll passes the real flags to the species constructor"
  );
  assert.sameValue(
    withGetter("flags", "gi", function () { return matchAllFlagsFor(/a/g); }),
    "gi",
    "@@matchAll passes a replaced `flags` getter to the species constructor"
  );
  assert.sameValue(
    withGetter("multiline", true, function () { return matchAllFlagsFor(/a/g); }),
    "gm",
    "@@matchAll observes a replaced individual flag getter"
  );
})();

// The fast path is guarded per realm, so tampering in one realm must not change the other.
(function () {
  var other = $262.createRealm();
  var OtherRegExp = other.global.RegExp;

  assert.sameValue(new OtherRegExp("a", "gi").flags, "gi", "foreign RegExp flags before tampering");
  assertAccepted(
    function () { return new OtherRegExp("a", "g"); },
    ["a", "a", "a"],
    "accepts a foreign global RegExp"
  );

  other.evalScript(
    "var savedGlobal = Object.getOwnPropertyDescriptor(RegExp.prototype, 'global');" +
    "Object.defineProperty(RegExp.prototype, 'global', {" +
    "  get: function () { return true; }, configurable: true });"
  );

  assert.sameValue(
    new OtherRegExp("a", "").flags,
    "g",
    "foreign RegExp uses its own realm's replaced `global` getter"
  );
  assertAccepted(
    function () { return new OtherRegExp("a", ""); },
    ["a", "a", "a"],
    "accepts a foreign non-global RegExp that its own realm reports as global"
  );
  assertOrdinaryUseUnaffected("(while the other realm is tampered with)");

  other.evalScript("Object.defineProperty(RegExp.prototype, 'global', savedGlobal);");

  assert.sameValue(
    new OtherRegExp("a", "").flags,
    "",
    "foreign RegExp flags once the other realm is restored"
  );
})();

// A replaced `flags` getter must still be called. Each method reads it once itself and once more
// from the RegExp method it delegates to, @@matchAll and @@replace respectively.
function countFlagsReads(callback) {
  var count = 0;
  var original = Object.getOwnPropertyDescriptor(RegExp.prototype, "flags");

  Object.defineProperty(RegExp.prototype, "flags", {
    get: function () {
      count++;
      return "g";
    },
    configurable: true,
  });

  try {
    callback();
  } finally {
    Object.defineProperty(RegExp.prototype, "flags", original);
  }

  return count;
}

assert.sameValue(
  countFlagsReads(function () { [...subject.matchAll(new RegExp("a", ""))]; }),
  2,
  "String.prototype.matchAll and @@matchAll each read the `flags` getter"
);
assert.sameValue(
  countFlagsReads(function () { subject.replaceAll(new RegExp("a", ""), "-"); }),
  2,
  "String.prototype.replaceAll and @@replace each read the `flags` getter"
);
