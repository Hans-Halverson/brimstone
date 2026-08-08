/*---
description: >
  RegExp.prototype[@@split] consumes the raw match of the builtin `exec` directly where possible.
includes: [compareArray.js]
---*/

var builtinExec = RegExp.prototype.exec;

// The splitter is constructed from the RegExp through @@species, so a subclass whose `exec` is a
// user-provided function forces @@split down the path that reads a match result array.
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

var splitPatterns = [
  ["b", ""],
  ["b", "g"],
  ["b", "y"],
  ["(b)", ""],
  ["(b)(c)", ""],
  ["(z)?(b)", ""],
  ["(?<name>b)", ""],
  ["(?<name>b)|(?<name>c)", ""],
  ["", ""],
  ["(?:)", ""],
  ["\\d", ""],
  ["(\\d)(\\d)?", ""],
  [".", "s"],
  ["B", "i"],
  ["^", "m"],
  ["$", "m"],
  ["b*$", ""],
];

var strings = ["", "a", "abc", "abcbd", "a1b22c", "bb", "a\u{1F600}b", "\n"];
var limits = [undefined, 0, 1, 2, 3, 100];

for (var i = 0; i < splitPatterns.length; i++) {
  for (var j = 0; j < strings.length; j++) {
    for (var k = 0; k < limits.length; k++) {
      assertSameSplit(splitPatterns[i][0], splitPatterns[i][1], strings[j], limits[k]);
    }
  }
}

// Capture groups are inserted into the result between the surrounding pieces.
assert.compareArray("a1b2c".split(new RegExp("(\\d)")), ["a", "1", "b", "2", "c"]);
assert.compareArray(
  "a1b".split(new RegExp("(\\d)(x)?")),
  ["a", "1", undefined, "b"]
);

// The limit is checked after each captured group is added, so it can cut a match's captures short.
assert.compareArray("a1b2c".split(new RegExp("(\\d)"), 1), ["a"]);
assert.compareArray("a1b2c".split(new RegExp("(\\d)"), 2), ["a", "1"]);
assert.compareArray("a1b2c".split(new RegExp("(\\d)"), 3), ["a", "1", "b"]);
assert.compareArray("a1b2c".split(new RegExp("(\\d)"), 4), ["a", "1", "b", "2"]);
assert.compareArray("a1b2c".split(new RegExp("(\\d)"), 0), []);
assert.compareArray(
  "a1b".split(new RegExp("(\\d)(x)?"), 3),
  ["a", "1", undefined]
);

// A pattern that matches the empty string splits between every code point, and is unicode aware
// when a unicode flag is set.
assert.compareArray("abc".split(new RegExp("")), ["a", "b", "c"]);
assert.compareArray("a\u{1F600}b".split(new RegExp("", "u")), ["a", "\u{1F600}", "b"]);
assert.compareArray("a\u{1F600}b".split(new RegExp("")), [
  "a",
  "\uD83D",
  "\uDE00",
  "b",
]);

// Splitting the empty string returns the empty string unless the pattern matches it.
assert.compareArray("".split(new RegExp("a")), [""]);
assert.compareArray("".split(new RegExp("")), []);
assert.compareArray("".split(new RegExp("(a)?")), []);

// A RegExp with no match leaves the string whole.
assert.compareArray("abc".split(new RegExp("z")), ["abc"]);
assert.compareArray("abc".split(new RegExp("(z)")), ["abc"]);

// The sticky flag is added to the splitter, so a non-sticky RegExp still splits at every match and
// an already sticky one behaves identically.
assert.compareArray("a1b2c".split(new RegExp("\\d", "y")), ["a", "b", "c"]);
assert.compareArray("a1b2c".split(new RegExp("\\d", "g")), ["a", "b", "c"]);

// Splitting must not disturb the original RegExp's last index.
var splitSource = new RegExp("\\d", "g");
splitSource.lastIndex = 4;
assert.compareArray("a1b2c".split(splitSource), ["a", "b", "c"]);
assert.sameValue(splitSource.lastIndex, 4, "@@split works on a separate splitter");

// @@species controls the splitter that is constructed, and a species returning a RegExp whose
// `exec` is user-provided still produces the same result.
var speciesCalls = 0;
class SpeciesRegExp extends RegExp {
  static get [Symbol.species]() {
    speciesCalls++;
    return RegExp;
  }
}
assert.compareArray("a1b2c".split(new SpeciesRegExp("(\\d)")), ["a", "1", "b", "2", "c"]);
assert.sameValue(speciesCalls, 1, "@@species is consulted once to build the splitter");
