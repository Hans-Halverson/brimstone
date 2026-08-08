/*---
description: RegExp.prototype[@@split] fast path if splitter is a RegExp with builtin `exec`.
includes: [compareArray.js]
---*/

var builtinExec = RegExp.prototype.exec;

// A subclass whose `exec` is a user-provided function is never eligible for the fast path, so it
// still matches the splitter at each index in turn. Its results are the reference the leftmost
// search is compared against.
function makeWrappedExecClass() {
  return class extends RegExp {
    exec(string) {
      return builtinExec.call(this, string);
    }
  };
}

function assertSameSplit(source, flags, string, limit) {
  var WrappedExecRegExp = makeWrappedExecClass();
  var message =
    "/" + source + "/" + flags +
    " on " + JSON.stringify(string) +
    " with limit " + String(limit);

  assert.compareArray(
    string.split(new RegExp(source, flags), limit),
    string.split(new WrappedExecRegExp(source, flags), limit),
    message
  );
}

// Anchors and patterns that can match the empty string at the end of the input. The leftmost search
// can find a match starting at the end of the string, which matching at each index never reaches
// because it stops before the final index.
var endOfStringPatterns = [
  ["$", ""],
  ["$", "m"],
  ["^", ""],
  ["^", "m"],
  ["b*$", ""],
  ["x*$", ""],
  ["(?=$)", ""],
  ["(?!x)$", ""],
  ["\\b", ""],
  ["\\B", ""],
  ["(?:)$", ""],
  ["c?$", ""],
];

var strings = ["", "a", "ab", "abc", "abcbd", "bb", "a\nb", "ab\n", "\n", "a\u{1F600}b"];
var limits = [undefined, 0, 1, 2, 3, 100];

for (var i = 0; i < endOfStringPatterns.length; i++) {
  for (var j = 0; j < strings.length; j++) {
    for (var k = 0; k < limits.length; k++) {
      assertSameSplit(endOfStringPatterns[i][0], endOfStringPatterns[i][1], strings[j], limits[k]);
    }
  }
}

// A match at the very end of the string is not a split point, so the input is left whole rather
// than gaining a trailing empty string.
assert.compareArray("ab".split(/$/), ["ab"], "$ does not split at the end of the input");
assert.compareArray("ab".split(/(?=$)/), ["ab"], "a lookahead at the end does not split");
assert.compareArray("ab".split(/x*$/), ["ab"], "an empty match at the end does not split");
assert.compareArray("ab".split(/b*$/), ["a", ""], "a non-empty match ending at the end splits");

// The splitter always has the sticky flag set, but is searched for the leftmost match, so the
// receiver's own flags do not change where the splits occur.
assert.compareArray("a1b2c".split(/\d/), ["a", "b", "c"], "no flags");
assert.compareArray("a1b2c".split(/\d/g), ["a", "b", "c"], "global receiver");
assert.compareArray("a1b2c".split(/\d/y), ["a", "b", "c"], "sticky receiver");
assert.compareArray("a1b2c".split(/\d/gy), ["a", "b", "c"], "global and sticky receiver");

// The receiver's last index is never consulted or written, since the splitter is a separate object.
var receiver = /\d/g;
receiver.lastIndex = 4;
assert.compareArray("a1b2c".split(receiver), ["a", "b", "c"], "receiver last index is ignored");
assert.sameValue(receiver.lastIndex, 4, "receiver last index is left alone");

// Replacing `exec` on the prototype must be observed, since the fast path stands in for a call to
// the builtin `exec`.
function withReplacedExec(replacement, callback) {
  RegExp.prototype.exec = replacement;

  try {
    return callback();
  } finally {
    RegExp.prototype.exec = builtinExec;
  }
}

withReplacedExec(function () { return null; }, function () {
  assert.compareArray("a1b2c".split(/\d/), ["a1b2c"], "a replaced `exec` that never matches");
});

assert.compareArray("a1b2c".split(/\d/), ["a", "b", "c"], "after `exec` is restored");

// The limit is coerced after the splitter is constructed, so its `valueOf` runs before the loop and
// can still replace `exec`.
var replacingLimit = {
  valueOf: function () {
    RegExp.prototype.exec = function () { return null; };
    return 100;
  },
};

try {
  assert.compareArray(
    "a1b2c".split(/\d/, replacingLimit),
    ["a1b2c"],
    "`exec` replaced while coercing the limit"
  );
} finally {
  RegExp.prototype.exec = builtinExec;
}

// The replacement must be the `exec` that is called, not just a reason to leave the fast path.
var calls = 0;
var countingLimit = {
  valueOf: function () {
    RegExp.prototype.exec = function (string) {
      calls++;
      return builtinExec.call(this, string);
    };
    return 100;
  },
};

try {
  assert.compareArray(
    "a1b2c".split(/\d/, countingLimit),
    ["a", "b", "c"],
    "an `exec` wrapper installed while coercing the limit"
  );
} finally {
  RegExp.prototype.exec = builtinExec;
}

assert.sameValue(calls > 0, true, "the `exec` installed while coercing the limit is called");

// An `exec` accessor on the prototype is observable, so it must still be called.
var originalExec = Object.getOwnPropertyDescriptor(RegExp.prototype, "exec");
var lookups = 0;
Object.defineProperty(RegExp.prototype, "exec", {
  configurable: true,
  get: function () {
    lookups++;
    return builtinExec;
  },
});

assert.compareArray("a1b2c".split(/\d/), ["a", "b", "c"], "prototype `exec` getter result");
assert.sameValue(lookups > 0, true, "prototype `exec` getter is called");

Object.defineProperty(RegExp.prototype, "exec", originalExec);

assert.compareArray("a1b2c".split(/\d/), ["a", "b", "c"], "after the `exec` getter is removed");

// The last index the spec's loop leaves on the splitter is observable whenever @@species hands back
// a splitter that user code kept a reference to, so such a splitter cannot take the fast path.
// The final match here ends exactly at the end of the string, which is the only case where the
// value differs from the zero a freshly constructed splitter starts at.
var leaked = null;
var leakingReceiver = /,/;
leakingReceiver.constructor = {
  [Symbol.species]: function (pattern, flags) {
    leaked = new RegExp(pattern, flags);
    return leaked;
  },
};

assert.compareArray("a,".split(leakingReceiver), ["a", ""], "a leaked splitter splits normally");
assert.sameValue(leaked.lastIndex, 2, "a leaked splitter keeps the last index the loop leaves");

// A species that returns a splitter matching a different pattern is still used as the splitter.
var otherReceiver = /,/;
otherReceiver.constructor = {
  [Symbol.species]: function () {
    return /\d/y;
  },
};

assert.compareArray("a1b,c".split(otherReceiver), ["a", "b,c"], "the species splitter is used");
