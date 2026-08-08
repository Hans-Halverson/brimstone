/*---
description: RegExp methods read and write `lastIndex` directly where possible.
includes: [compareArray.js]
---*/

// Every operation that reads or writes `lastIndex`, reporting the value left behind. The RegExp is
// created by the caller so that receivers in different states can be compared.
function runAll(makeRegExp, string) {
  var forExec = makeRegExp();
  var forTest = makeRegExp();
  var forSearch = makeRegExp();
  forSearch.lastIndex = 2;

  return {
    exec: JSON.stringify(forExec.exec(string)),
    execLastIndex: forExec.lastIndex,
    test: forTest.test(string),
    testLastIndex: forTest.lastIndex,
    search: string.search(forSearch),
    searchLastIndex: forSearch.lastIndex,
    match: JSON.stringify(string.match(makeRegExp())),
    matchAll: makeRegExp().global ? JSON.stringify([...string.matchAll(makeRegExp())]) : "",
    replace: string.replace(makeRegExp(), "<$&>"),
    split: string.split(makeRegExp()),
  };
}

function assertSameResults(makeSlow, source, flags, string, receiverName) {
  var message =
    "/" + source + "/" + flags +
    " on " + JSON.stringify(string) +
    " (" + receiverName + ")";

  var fast = runAll(function () { return new RegExp(source, flags); }, string);
  var slow = runAll(function () { return makeSlow(source, flags); }, string);

  assert.sameValue(fast.exec, slow.exec, message + " exec");
  assert.sameValue(fast.execLastIndex, slow.execLastIndex, message + " exec last index");
  assert.sameValue(fast.test, slow.test, message + " test");
  assert.sameValue(fast.testLastIndex, slow.testLastIndex, message + " test last index");
  assert.sameValue(fast.search, slow.search, message + " search");
  assert.sameValue(fast.searchLastIndex, slow.searchLastIndex, message + " search last index");
  assert.sameValue(fast.match, slow.match, message + " match");
  assert.sameValue(fast.matchAll, slow.matchAll, message + " matchAll");
  assert.sameValue(fast.replace, slow.replace, message + " replace");
  assert.compareArray(fast.split, slow.split, message + " split");
}

// A RegExp with enough own properties to leave the shape's array mode, so `lastIndex` no longer has
// a fixed location and every access goes through the ordinary path.
function inMapMode(source, flags) {
  var regexp = new RegExp(source, flags);
  for (var i = 0; i < 64; i++) {
    regexp["property" + i] = i;
  }

  // Deleting a property also forces map mode
  delete regexp.property0;

  return regexp;
}

// A RegExp that is somebody's prototype. Becoming a prototype is not by itself enough: the shape is
// cloned into a per-object prototype shape only when an inline cache first caches a lookup through
// it, so the inherited read below is what actually moves `regexp` off the common shape.
function asPrototype(source, flags) {
  var regexp = new RegExp(source, flags);
  var child = Object.create(regexp);
  void child.lastIndex;

  return regexp;
}

var sources = ["a", "(a)b", "\u{1F600}"];
var flagCombinations = ["", "g", "y", "gy", "gu", "dgimsy"];
var strings = ["", "abc", "aXbXc", "\u{1F600}a\u{1F600}"];

for (var i = 0; i < sources.length; i++) {
  for (var j = 0; j < flagCombinations.length; j++) {
    for (var k = 0; k < strings.length; k++) {
      assertSameResults(inMapMode, sources[i], flagCombinations[j], strings[k], "map mode");
      assertSameResults(asPrototype, sources[i], flagCombinations[j], strings[k], "prototype");
    }
  }
}

// A last index that is not a small integer must still be converted by ToLength.
var coercionTarget = "abcd";
var coercions = [
  ["1", 1],
  [1.9, 1],
  [-1, 0],
  [-0.5, 0],
  [NaN, 0],
  [-Infinity, 0],
  [null, 0],
  [undefined, 0],
  [false, 0],
  [true, 1],
  [{ valueOf: function () { return 1; } }, 1],
  [[2], 2],
  ["3", 3],
  // Values at and beyond the ToLength maximum, which straddle the fast path's range check
  [Math.pow(2, 53) - 1, null],
  [Math.pow(2, 53), null],
  [Math.pow(2, 53) + 2, null],
  [Infinity, null],
  [1e21, null],
  ["1e21", null],
];

for (var i = 0; i < coercions.length; i++) {
  var sticky = new RegExp(".", "y");
  sticky.lastIndex = coercions[i][0];

  var expectedIndex = coercions[i][1];
  var match = sticky.exec(coercionTarget);
  var message = " for a last index of " + String(coercions[i][0]);

  if (expectedIndex === null) {
    assert.sameValue(match, null, "no sticky match" + message);
    assert.sameValue(sticky.lastIndex, 0, "resulting last index" + message);
  } else {
    assert.notSameValue(match, null, "sticky match" + message);
    assert.sameValue(match.index, expectedIndex, "match index" + message);
    assert.sameValue(sticky.lastIndex, expectedIndex + 1, "resulting last index" + message);
  }
}

// A last index past the end of the string always fails and is reset.
var pastEnd = new RegExp("a", "g");
pastEnd.lastIndex = 100;
assert.sameValue(pastEnd.exec("abc"), null, "exec with a last index past the end");
assert.sameValue(pastEnd.lastIndex, 0, "last index reset after failing past the end");

// @@search restores the exact value it found, not a coerced one, and does so without writing when
// the value is already zero.
var searchValues = ["abc", 1.5, -3, NaN, null];
for (var i = 0; i < searchValues.length; i++) {
  var searched = new RegExp("b", "g");
  searched.lastIndex = searchValues[i];

  assert.sameValue("abc".search(searched), 1, "search result for " + String(searchValues[i]));
  assert.sameValue(
    searched.lastIndex,
    searchValues[i],
    "search restores the exact last index " + String(searchValues[i])
  );
}

// A non-writable last index makes every write fail, which is a TypeError for a global RegExp.
var nonWritable = new RegExp("a", "g");
Object.defineProperty(nonWritable, "lastIndex", { writable: false });
assert.sameValue(nonWritable.lastIndex, 0, "non-writable last index keeps its value");
assert.throws(TypeError, function () {
  nonWritable.exec("aa");
}, "exec on a global RegExp with a non-writable last index");
assert.throws(TypeError, function () {
  "aa".replace(nonWritable, "-");
}, "@@replace with a non-writable last index");

// A non-global, non-sticky RegExp never writes its last index, so it does not throw.
var nonWritableLocal = new RegExp("a", "");
Object.defineProperty(nonWritableLocal, "lastIndex", { writable: false });
assert.compareArray(nonWritableLocal.exec("aa"), ["a"], "exec that never writes the last index");
assert.sameValue(nonWritableLocal.lastIndex, 0, "last index of a non-global RegExp is untouched");

// A RegExp used as a prototype has its last index read through the prototype chain, so a write must
// be visible to everything that inherits it. The repeated reads give any inline cache a chance to
// settle on the value it saw first.
var protoRegExp = new RegExp("a", "g");
var child = Object.create(protoRegExp);
for (var i = 0; i < 20; i++) {
  assert.sameValue(child.lastIndex, 0, "inherited last index before matching");
}

protoRegExp.exec("aa");
assert.sameValue(protoRegExp.lastIndex, 1, "last index after matching");

for (var i = 0; i < 20; i++) {
  assert.sameValue(child.lastIndex, 1, "inherited last index after matching");
}

// The matchAll iterator advances the last index itself when it matches the empty string.
var emptyMatches = [..."ab".matchAll(new RegExp("", "g"))];
assert.sameValue(emptyMatches.length, 3, "matchAll over an empty pattern terminates");
assert.compareArray(
  emptyMatches.map(function (match) { return match.index; }),
  [0, 1, 2],
  "matchAll empty match indices"
);

var unicodeMatches = [..."\u{1F600}a".matchAll(new RegExp("", "gu"))];
assert.compareArray(
  unicodeMatches.map(function (match) { return match.index; }),
  [0, 2, 3],
  "matchAll advances by whole code points when unicode"
);

// Constructing a RegExp resets the last index, including when reusing an existing RegExp.
var reset = new RegExp("a", "g");
reset.lastIndex = 5;
assert.sameValue(new RegExp(reset).lastIndex, 0, "a copied RegExp starts at zero");
assert.sameValue(new RegExp(reset, "y").lastIndex, 0, "a copied RegExp with flags starts at zero");

// A subclass instance still has its own last index in the usual place.
class Subclass extends RegExp {}
var subclass = new Subclass("a", "g");
assert.compareArray(subclass.exec("xa"), ["a"], "subclass exec");
assert.sameValue(subclass.lastIndex, 2, "subclass last index");
assert.sameValue("aa".replace(subclass, "-"), "--", "subclass replace");

// A plain object standing in for a RegExp goes through the ordinary property path throughout.
var plain = {
  flags: "g",
  global: true,
  lastIndex: 0,
  exec: function (string) {
    if (this.lastIndex >= string.length) {
      this.lastIndex = 0;
      return null;
    }

    var index = this.lastIndex;
    this.lastIndex = index + 1;

    return { 0: string[index], index: index, length: 1 };
  },
};

assert.sameValue(
  RegExp.prototype[Symbol.replace].call(plain, "abc", "-"),
  "---",
  "@@replace on a plain object with its own last index"
);
assert.sameValue(plain.lastIndex, 0, "plain object last index after @@replace");

// A proxy has no own last index of its own, so every access must go through its traps.
var trapped = [];
var proxied = new Proxy(plain, {
  get: function (target, key, receiver) {
    trapped.push("get " + String(key));
    return Reflect.get(target, key, receiver);
  },
  set: function (target, key, value, receiver) {
    trapped.push("set " + String(key));
    return Reflect.set(target, key, value, receiver);
  },
});

assert.sameValue(
  RegExp.prototype[Symbol.replace].call(proxied, "abc", "-"),
  "---",
  "@@replace on a proxied RegExp stand-in"
);
assert.sameValue(
  trapped.indexOf("set lastIndex") !== -1,
  true,
  "the proxy set trap is called for the last index"
);
assert.sameValue(
  trapped.indexOf("get lastIndex") !== -1,
  true,
  "the proxy get trap is called for the last index"
);
