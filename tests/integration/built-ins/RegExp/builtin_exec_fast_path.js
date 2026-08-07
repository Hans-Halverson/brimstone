/*---
description: >
  RegExpExec calls the builtin `exec` inline to obtain the raw match where possible Verify that we
  get the same results using the raw match directly vs using the full match object when a
  user-defined `exec` methods is used.
includes: [compareArray.js]
---*/

var builtinExec = RegExp.prototype.exec;

// A RegExp equivalent to the argument, but whose `exec` is a user-provided function. This forces
// RegExpExec down the generic path where the match result array must be built and read back.
function withWrappedExec(source, flags) {
  var regexp = new RegExp(source, flags);
  regexp.exec = function (string) {
    return builtinExec.call(this, string);
  };

  return regexp;
}

// Every operation built on RegExpExec, applied to a freshly created RegExp so that no last index
// state is carried between them. String.prototype.matchAll requires a global RegExp.
function runAll(makeRegExp, string) {
  return {
    test: makeRegExp().test(string),
    search: string.search(makeRegExp()),
    match: string.match(makeRegExp()),
    matchAll: makeRegExp().global ? [...string.matchAll(makeRegExp())] : [],
    replaceString: string.replace(makeRegExp(), "<$&|$1|$2>"),
    replaceFunction: string.replace(makeRegExp(), function () {
      return "<" + Array.prototype.slice.call(arguments, 0, -2).join(",") + ">";
    }),
    split: string.split(makeRegExp()),
    splitLimited: string.split(makeRegExp(), 3),
    exec: makeRegExp().exec(string),
  };
}

function assertSameResults(source, flags, string) {
  var message = "/" + source + "/" + flags + " on " + JSON.stringify(string);

  var inline = runAll(function () { return new RegExp(source, flags); }, string);
  var wrapped = runAll(function () { return withWrappedExec(source, flags); }, string);

  assert.sameValue(inline.test, wrapped.test, message + " test");
  assert.sameValue(inline.search, wrapped.search, message + " search");
  assert.sameValue(inline.replaceString, wrapped.replaceString, message + " replace with string");
  assert.sameValue(
    inline.replaceFunction,
    wrapped.replaceFunction,
    message + " replace with function"
  );
  assert.compareArray(inline.split, wrapped.split, message + " split");
  assert.compareArray(inline.splitLimited, wrapped.splitLimited, message + " split with limit");

  assert.sameValue(
    JSON.stringify(inline.match),
    JSON.stringify(wrapped.match),
    message + " match"
  );
  assert.sameValue(
    JSON.stringify(inline.matchAll),
    JSON.stringify(wrapped.matchAll),
    message + " matchAll"
  );
  assert.sameValue(
    JSON.stringify(inline.exec),
    JSON.stringify(wrapped.exec),
    message + " exec"
  );
}

// Patterns covering no captures, indexed captures, optional captures that do not participate in the
// match, named captures, and empty matches.
var patterns = [
  ["b", ""],
  ["b", "g"],
  ["(b)(c)", "g"],
  ["(z)?(b)", "g"],
  ["(?<first>b)(?<second>c)", "g"],
  ["(?<first>b)|(?<first>c)", "g"],
  ["", "g"],
  ["(?:)", "g"],
  ["b", "gy"],
  ["b", "y"],
  ["(b)(c)", "gd"],
  ["B(C)", "gi"],
  [".", "gs"],
  ["^.", "gm"],
  [".", "gu"],
  ["\\d", "g"],
];

var strings = ["", "abc", "abcbc", "abc\ndbc", "a\u{1F600}bc", "aaa"];

for (var i = 0; i < patterns.length; i++) {
  for (var j = 0; j < strings.length; j++) {
    assertSameResults(patterns[i][0], patterns[i][1], strings[j]);
  }
}

// The inline path is also taken when `exec` is an own property that happens to be the builtin, and
// when it is inherited by a subclass instance.
var ownBuiltinExec = new RegExp("(b)", "g");
ownBuiltinExec.exec = builtinExec;
assert.sameValue("abcb".replace(ownBuiltinExec, "<$1>"), "a<b>c<b>");

class SubRegExp extends RegExp {}
var subclassed = new SubRegExp("(b)", "g");
assert.sameValue(subclassed.exec, builtinExec);
assert.sameValue("abcb".replace(subclassed, "<$1>"), "a<b>c<b>");
assert.sameValue(subclassed.test("abc"), true);
assert.compareArray("abcb".split(new SubRegExp("(b)")), ["a", "b", "c", "b", ""]);

// A bound builtin `exec` is a different function, so the generic path is used instead.
var boundExec = new RegExp("(b)", "g");
boundExec.exec = builtinExec.bind(boundExec);
assert.sameValue("abcb".replace(boundExec, "<$1>"), "a<b>c<b>");

// A Proxy is not a RegExp, so the generic path is used even though `exec` is the builtin. The
// builtin `exec` is then called with the Proxy as its receiver and rejects it.
var proxied = new Proxy(new RegExp("(b)", "g"), {});
assert.sameValue(proxied.exec, builtinExec);
assert.throws(TypeError, function () {
  RegExp.prototype.test.call(proxied, "abc");
});

// An ordinary object carrying the builtin `exec` is not a RegExp either, and is rejected the same
// way rather than being matched against.
assert.throws(TypeError, function () {
  RegExp.prototype.test.call({ exec: builtinExec }, "abc");
});

// A Proxy wrapping the builtin `exec` is a different function, so the generic path is used and the
// call is forwarded through the Proxy.
var proxiedExec = new RegExp("(b)", "g");
proxiedExec.exec = new Proxy(builtinExec, {});
assert.sameValue("abcb".replace(proxiedExec, "<$1>"), "a<b>c<b>");

// `exec` is looked up once per match attempt, even when it resolves to the builtin.
function countExecLookups(regexp, run) {
  var lookups = 0;
  Object.defineProperty(regexp, "exec", {
    configurable: true,
    get: function () {
      lookups++;
      return builtinExec;
    },
  });

  run(regexp);

  return lookups;
}

// Three matches in "abcb" plus the final failed attempt.
assert.sameValue(
  countExecLookups(new RegExp("b", "g"), function (regexp) {
    assert.compareArray("abcb".match(regexp), ["b", "b"]);
  }),
  3,
  "@@match looks up `exec` once per attempt"
);

assert.sameValue(
  countExecLookups(new RegExp("b", "g"), function (regexp) {
    assert.sameValue("abcb".replace(regexp, "X"), "aXcX");
  }),
  3,
  "@@replace looks up `exec` once per attempt"
);

assert.sameValue(
  countExecLookups(new RegExp("b"), function (regexp) {
    assert.sameValue(regexp.test("abc"), true);
  }),
  1,
  "test looks up `exec` once"
);

assert.sameValue(
  countExecLookups(new RegExp("b"), function (regexp) {
    assert.sameValue("abc".search(regexp), 1);
  }),
  1,
  "@@search looks up `exec` once"
);

// Last index is still maintained when the builtin `exec` runs inline. It advances past each match,
// resumes from wherever it was left, and resets once the match fails.
var tracked = new RegExp("b", "g");
assert.sameValue(tracked.test("abcb"), true);
assert.sameValue(tracked.lastIndex, 2, "last index advances past the match");
assert.sameValue(tracked.test("abcb"), true);
assert.sameValue(tracked.lastIndex, 4, "matching resumes from the stored last index");
assert.sameValue(tracked.test("abcb"), false);
assert.sameValue(tracked.lastIndex, 0, "last index resets when the match fails");

// A last index past the end of the string fails without running the matcher.
tracked.lastIndex = 5;
assert.sameValue(tracked.test("abcb"), false);
assert.sameValue(tracked.lastIndex, 0, "an out of range last index resets");

// A sticky RegExp only matches at last index.
var sticky = new RegExp("b", "y");
sticky.lastIndex = 1;
assert.sameValue(sticky.test("abcb"), true);
assert.sameValue(sticky.lastIndex, 2);
assert.sameValue(sticky.test("abcb"), false, "no match at the new last index");
assert.sameValue(sticky.lastIndex, 0);

// A non-global, non-sticky RegExp never writes last index and always matches from the start.
var nonGlobal = new RegExp("b");
nonGlobal.lastIndex = 3;
assert.sameValue(nonGlobal.test("abcb"), true);
assert.sameValue(nonGlobal.lastIndex, 3, "last index is untouched without the g or y flag");

// @@search restores the original last index rather than leaving the match's.
var searched = new RegExp("b", "g");
searched.lastIndex = 3;
assert.sameValue("abcb".search(searched), 1);
assert.sameValue(searched.lastIndex, 3, "@@search restores last index");

// Swapping `exec` part way through a loop must switch to the swapped-in function. The first attempt
// uses the builtin and each later attempt returns the next of the given results.
function swapExecAfterFirst(laterResults) {
  var regexp = new RegExp("b", "g");
  var attempts = 0;
  Object.defineProperty(regexp, "exec", {
    configurable: true,
    get: function () {
      attempts++;
      if (attempts === 1) {
        return builtinExec;
      }

      var result = laterResults[attempts - 2];
      return function () {
        if (result === null || result === undefined) {
          return null;
        }

        this.lastIndex = result.index + result[0].length;
        return result;
      };
    },
  });

  return regexp;
}

assert.compareArray(
  "abcb".match(swapExecAfterFirst([null])),
  ["b"],
  "@@match stops once the swapped-in `exec` reports no match"
);

assert.sameValue(
  "abcb".replace(swapExecAfterFirst([{ 0: "c", index: 2, length: 1 }, null]), "<$&>"),
  "a<b><c>b",
  "@@replace uses the match reported by the swapped-in `exec`"
);

// A swapped-in `exec` returning something other than an object or null is a TypeError.
var badExec = new RegExp("b", "g");
badExec.exec = function () {
  return 5;
};
assert.throws(TypeError, function () {
  badExec.test("abcb");
});

// An `exec` property that is not callable is ignored, and the builtin RegExpBuiltinExec is used
// instead. This is only reachable on a real RegExp; anything else is a TypeError.
var nonCallableValues = [5, "exec", null, undefined, true, Symbol("exec"), {}, []];
for (var i = 0; i < nonCallableValues.length; i++) {
  var nonCallable = new RegExp("(b)", "g");
  nonCallable.exec = nonCallableValues[i];

  assert.sameValue(nonCallable.test("abcb"), true, "test with a non-callable `exec`");
  nonCallable.lastIndex = 0;
  assert.sameValue("abc".search(nonCallable), 1, "@@search with a non-callable `exec`");
  nonCallable.lastIndex = 0;
  assert.compareArray("abcb".match(nonCallable), ["b", "b"], "@@match with a non-callable `exec`");
  nonCallable.lastIndex = 0;
  assert.sameValue(
    "abcb".replace(nonCallable, "<$1>"),
    "a<b>c<b>",
    "@@replace with a non-callable `exec`"
  );
  nonCallable.lastIndex = 0;
  assert.compareArray(
    "abcb".split(nonCallable),
    ["a", "b", "c", "b", ""],
    "@@split with a non-callable `exec`"
  );
}

assert.throws(TypeError, function () {
  RegExp.prototype.test.call({ exec: 5 }, "abc");
});

// A user-provided `exec` may report a match with arbitrary property values. @@search returns the
// `index` property without converting it, while @@match converts the `0` property to a string.
var customIndex = new RegExp("b");
customIndex.exec = function () {
  return { index: "not a number" };
};
assert.sameValue("abc".search(customIndex), "not a number", "@@search returns `index` unconverted");

var customMatched = new RegExp("b");
customMatched.exec = function () {
  return { 0: 42 };
};
assert.sameValue("abc".match(customMatched)[0], 42, "a non-global @@match returns the object");

var customMatchedGlobal = new RegExp("b", "g");
var globalAttempts = 0;
customMatchedGlobal.exec = function () {
  globalAttempts++;
  return globalAttempts === 1 ? { 0: 42 } : null;
};
assert.compareArray(
  "abc".match(customMatchedGlobal),
  ["42"],
  "a global @@match converts the matched value to a string"
);

// Errors thrown out of a user-provided `exec` propagate.
var thrownError = new Error("from exec");
assert.throws(Error, function () {
  var regexp = new RegExp("b", "g");
  regexp.exec = function () {
    throw thrownError;
  };
  "abcb".replace(regexp, "X");
});

// Reassigning RegExp.prototype.exec makes every method go through the replacement.
var savedExec = RegExp.prototype.exec;
try {
  RegExp.prototype.exec = function () {
    return null;
  };

  assert.sameValue(new RegExp("b", "g").test("abcb"), false);
  assert.sameValue("abcb".search(new RegExp("b")), -1);
  assert.sameValue("abcb".match(new RegExp("b", "g")), null);
  assert.sameValue("abcb".replace(new RegExp("b", "g"), "X"), "abcb");
  assert.compareArray("abcb".split(new RegExp("b")), ["abcb"]);
} finally {
  RegExp.prototype.exec = savedExec;
}

assert.sameValue(new RegExp("b", "g").test("abcb"), true, "the builtin is restored");