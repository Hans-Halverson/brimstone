/*---
description: >
  RegExpExec skips looking up `exec` entirely when it is known to be the builtin on the RegExp
  prototype. Verify that replacing, shadowing, deleting, or restoring it is still observed, and that
  the lookup is still performed whenever it could be intercepted.
includes: [compareArray.js]
---*/

var builtinExec = RegExp.prototype.exec;

// Every operation built on RegExpExec, applied to a freshly created RegExp so that no last index
// state is carried between them.
function runAll(string) {
  return {
    test: new RegExp("b", "g").test(string),
    search: string.search(new RegExp("b", "g")),
    match: JSON.stringify(string.match(new RegExp("b", "g"))),
    matchAll: JSON.stringify([...string.matchAll(new RegExp("b", "g"))]),
    replace: string.replace(new RegExp("b", "g"), "<$&>"),
    split: string.split(new RegExp("b", "g")),
    exec: JSON.stringify(new RegExp("b", "g").exec(string)),
  };
}

function assertAllUseBuiltin(message) {
  var results = runAll("abcb");

  assert.sameValue(results.test, true, message + " test");
  assert.sameValue(results.search, 1, message + " search");
  assert.sameValue(results.match, '["b","b"]', message + " match");
  assert.sameValue(results.replace, "a<b>c<b>", message + " replace");
  assert.compareArray(results.split, ["a", "c", ""], message + " split");
  assert.sameValue(results.matchAll, '[["b"],["b"]]', message + " matchAll");
  assert.sameValue(results.exec, '["b"]', message + " exec");
}

// Run first so that the fast path is taken and its guard is established before it is invalidated
// below. Every case afterwards must be observed even though a guard is already in place.
assertAllUseBuiltin("with the builtin `exec`");

// Replace `exec` on the prototype for the duration of a callback, by plain assignment. Assigning to
// an existing data property does not change the prototype's shape, so this is the case most likely
// to be missed by a stale guard.
function withReplacedExec(replacement, callback) {
  RegExp.prototype.exec = replacement;

  try {
    return callback();
  } finally {
    RegExp.prototype.exec = builtinExec;
  }
}

var neverMatches = function () {
  return null;
};

withReplacedExec(neverMatches, function () {
  assert.sameValue(new RegExp("b", "g").test("abcb"), false, "replaced `exec` is used by test");
  assert.sameValue("abcb".search(new RegExp("b", "g")), -1, "replaced `exec` is used by @@search");
  assert.sameValue("abcb".match(new RegExp("b", "g")), null, "replaced `exec` is used by @@match");
  assert.sameValue(
    "abcb".replace(new RegExp("b", "g"), "X"),
    "abcb",
    "replaced `exec` is used by @@replace"
  );
  assert.compareArray(
    "abcb".split(new RegExp("b", "g")),
    ["abcb"],
    "replaced `exec` is used by @@split"
  );
});

assertAllUseBuiltin("after `exec` is restored");

// Defining `exec` with a descriptor rather than assigning it is observed the same way.
var originalExec = Object.getOwnPropertyDescriptor(RegExp.prototype, "exec");
Object.defineProperty(RegExp.prototype, "exec", { value: neverMatches, configurable: true });
assert.sameValue(new RegExp("b", "g").test("abcb"), false, "defined `exec` is used");
Object.defineProperty(RegExp.prototype, "exec", originalExec);

assertAllUseBuiltin("after the `exec` descriptor is restored");

// An `exec` accessor on the prototype must be called, since a getter is observable.
var lookups = 0;
Object.defineProperty(RegExp.prototype, "exec", {
  configurable: true,
  get: function () {
    lookups++;
    return builtinExec;
  },
});

// Two matches in "abcb" plus the final failed attempt.
assert.sameValue("abcb".replace(new RegExp("b", "g"), "X"), "aXcX", "prototype `exec` getter result");
assert.sameValue(lookups, 3, "prototype `exec` getter is called once per attempt");

Object.defineProperty(RegExp.prototype, "exec", originalExec);

assertAllUseBuiltin("after the `exec` getter is removed");

// Deleting `exec` leaves no callable to find, so RegExpBuiltinExec is performed directly.
delete RegExp.prototype.exec;
assert.sameValue(new RegExp("b", "g").test("abcb"), true, "test with no `exec` at all");
assert.sameValue("abcb".replace(new RegExp("b", "g"), "X"), "aXcX", "@@replace with no `exec`");
Object.defineProperty(RegExp.prototype, "exec", originalExec);

assertAllUseBuiltin("after `exec` is added back");

// A change to the RegExp prototype that leaves `exec` alone must not change any results.
RegExp.prototype.unrelatedProperty = 1;
assertAllUseBuiltin("after an unrelated property is added");
delete RegExp.prototype.unrelatedProperty;
assertAllUseBuiltin("after an unrelated property is removed");

// An own `exec` shadows the prototype's, so the lookup must still find it.
var shadowed = new RegExp("b", "g");
shadowed.exec = neverMatches;
assert.sameValue("abcb".replace(shadowed, "X"), "abcb", "own `exec` shadows the prototype");

// A receiver that is not on the RegExp common shape still inherits the builtin `exec` and matches
// the same way, reached through the ordinary lookup instead.
var withOwnProperty = new RegExp("b", "g");
withOwnProperty.marker = 1;
assert.sameValue(
  "abcb".replace(withOwnProperty, "X"),
  "aXcX",
  "an extra own property does not change the result"
);

// Writing the last index does not add a property, so the fast path still applies.
var withLastIndex = new RegExp("b", "g");
withLastIndex.lastIndex = 3;
assert.sameValue(withLastIndex.test("abcb"), true, "test resuming from a written last index");
assert.sameValue(withLastIndex.lastIndex, 4, "last index after matching");

// The fast path is guarded per realm, and only accepts an `exec` belonging to the realm whose
// methods are running, so tampering in one realm must not change the other.
(function () {
  var other = $262.createRealm();
  var OtherRegExp = other.global.RegExp;

  // Take the fast path in both realms so that each has established its guard
  assertAllUseBuiltin("before the other realm is tampered with");
  other.evalScript('var replaced = "abcb".replace(/b/g, "X");');
  assert.sameValue(other.global.replaced, "aXcX", "the other realm uses its own builtin `exec`");

  other.evalScript(
    "var savedExec = RegExp.prototype.exec;" +
    "RegExp.prototype.exec = function () { return null; };"
  );

  assertAllUseBuiltin("while the other realm is tampered with");
  other.evalScript('replaced = "abcb".replace(/b/g, "X");');
  assert.sameValue(other.global.replaced, "abcb", "the other realm observes its own replaced `exec`");

  // A foreign RegExp is matched by its own realm's methods, so it uses the replacement as well.
  assert.sameValue(
    "abcb".replace(new OtherRegExp("b", "g"), "X"),
    "abcb",
    "a foreign RegExp uses its own realm's replaced `exec`"
  );

  other.evalScript("RegExp.prototype.exec = savedExec;");

  assertAllUseBuiltin("once the other realm is restored");
  assert.sameValue(
    "abcb".replace(new OtherRegExp("b", "g"), "X"),
    "aXcX",
    "a foreign RegExp once its own realm is restored"
  );

  // The other realm's builtin `exec` is a different function to this realm's, so the lookup is
  // still performed and the call builds its match result array in the realm of the `exec` that ran.
  withReplacedExec(OtherRegExp.prototype.exec, function () {
    assert.sameValue(
      Object.getPrototypeOf("abcb".match(new RegExp("b", ""))),
      other.global.Array.prototype,
      "a foreign builtin `exec` builds its match result in its own realm"
    );
  });
})();
