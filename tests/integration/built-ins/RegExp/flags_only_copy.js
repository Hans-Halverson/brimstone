/*---
description: >
  Constructing a RegExp from an existing RegExp plus a flags argument may reuse compiled bytecode.
includes: [compareArray.js]
---*/

// Flags that do not affect compilation, so the copy shares the source regexp's bytecode.
var runtimeOnlyFlags = ["", "d", "g", "y", "dg", "dy", "gy", "dgy"];

// Flags that do affect compilation, so the pattern must be reparsed and recompiled.
var compilationFlags = ["i", "m", "s", "u", "v", "im", "gis", "dgimsy"];

var matchStrings = ["", "abc", "ABC", "AzB", "abcabc", "a\nb", "x\nazb", "aa/bb", "a\u{1F600}b"];

// The canonical flags string does not depend on the pattern.
var canonicalFlagsCache = {};
function canonicalFlags(flags) {
  if (!(flags in canonicalFlagsCache)) {
    canonicalFlagsCache[flags] = new RegExp("a", flags).flags;
  }
  return canonicalFlagsCache[flags];
}

// The source is always taken from the source regexp, and the flags always from the argument,
// however the compiled regexp is obtained. The original and direct regexps are reused across
// calls, so reset lastIndex before each use to make them behave like freshly constructed ones.
function assertCopy(original, direct, source, sourceFlags, newFlags) {
  var copy = new RegExp(original, newFlags);
  var message = "/" + source + "/" + sourceFlags + " with " + JSON.stringify(newFlags);

  assert.sameValue(copy.source, original.source, message + " source");
  assert.sameValue(copy.flags, canonicalFlags(newFlags), message + " flags");
  assert.sameValue(copy.lastIndex, 0, message + " last index");
  assert.notSameValue(copy, original, message + " is a new object");

  // Matching must agree with a regexp compiled from the same source and flags directly.
  for (var i = 0; i < matchStrings.length; i++) {
    copy.lastIndex = 0;
    direct.lastIndex = 0;
    assert.sameValue(
      JSON.stringify(copy.exec(matchStrings[i])),
      JSON.stringify(direct.exec(matchStrings[i])),
      message + " exec on " + JSON.stringify(matchStrings[i])
    );
  }
}

var sources = ["abc", "(a)(b)", "(?<name>a)b", "a|b", "[a-c]+", "a\\/b", "\\d{2,3}", "^a.b$"];
var allFlags = runtimeOnlyFlags.concat(compilationFlags);

// Whether the copy reuses the compiled regexp as is, clones its bytecode with new flags, or
// recompiles from scratch depends only on the pair of flags, never on the pattern. Run the full
// matrix of flag pairs against a single pattern whose matching is sensitive to each compilation
// flag, so a copy that wrongly shares bytecode shows up in the exec results.
var matrixSource = "^(?<name>a).b$";

var matrixOriginals = [];
for (var j = 0; j < allFlags.length; j++) {
  matrixOriginals.push(new RegExp(matrixSource, allFlags[j]));
}

for (var k = 0; k < allFlags.length; k++) {
  var direct = new RegExp(matrixOriginals[0].source, allFlags[k]);
  for (var j = 0; j < allFlags.length; j++) {
    assertCopy(matrixOriginals[j], direct, matrixSource, allFlags[j], allFlags[k]);
  }
}

// Run every pattern against one pair of flags from each reuse class: identical flags, a runtime
// only difference, a runtime only difference alongside equal compilation flags, and gaining or
// losing a compilation flag.
//
// Every compilation flag must appear in some pair so that each pattern is parsed under it, since
// the matrix pattern above cannot stand in for the parse. `v` in particular parses character
// classes through a separate path and the matrix pattern has no character class, so the last pair
// recompiles every pattern in unicode sets mode.
var flagPairs = [
  ["dgy", "dgy"],
  ["g", "dy"],
  ["gis", "dgis"],
  ["", "im"],
  ["iu", "g"],
  ["g", "v"],
];

for (var i = 0; i < sources.length; i++) {
  for (var p = 0; p < flagPairs.length; p++) {
    var original = new RegExp(sources[i], flagPairs[p][0]);
    var direct = new RegExp(original.source, flagPairs[p][1]);
    assertCopy(original, direct, sources[i], flagPairs[p][0], flagPairs[p][1]);
  }
}

// Copying with identical flags reuses the compiled regexp, which must not disturb the source.
var shared = new RegExp("(a)b", "g");
shared.lastIndex = 3;
var sharedCopy = new RegExp(shared, "g");
assert.sameValue(sharedCopy.lastIndex, 0, "the copy starts with a fresh last index");
assert.sameValue(shared.lastIndex, 3, "the source keeps its last index");
assert.compareArray(sharedCopy.exec("xabx"), ["ab", "a"]);
assert.sameValue(shared.lastIndex, 3, "matching the copy does not touch the source");

// The copy's own flags drive matching even though the bytecode is shared.
var caseSource = new RegExp("abc", "i");
assert.sameValue(new RegExp(caseSource, "i").test("ABC"), true);
assert.sameValue(new RegExp(caseSource, "").test("ABC"), false, "dropping `i` recompiles");
assert.sameValue(new RegExp(new RegExp("abc", ""), "i").test("ABC"), true, "adding `i` recompiles");

// Adding `y` to a copy makes it sticky even though the bytecode is unchanged.
var stickyCopy = new RegExp(new RegExp("b", "g"), "y");
stickyCopy.lastIndex = 1;
assert.sameValue(stickyCopy.test("abc"), true);
stickyCopy.lastIndex = 0;
assert.sameValue(stickyCopy.test("abc"), false, "the copy honours its own sticky flag");

// Dropping `y` from a copy makes it non-sticky.
var unstickyCopy = new RegExp(new RegExp("b", "y"), "");
assert.sameValue(unstickyCopy.test("abc"), true);

// Adding `d` to a copy produces match indices even though the bytecode is unchanged.
var indicesCopy = new RegExp(new RegExp("(?<name>b)(c)", ""), "d");
var indicesMatch = indicesCopy.exec("abcd");
assert.compareArray(indicesMatch.indices[0], [1, 3]);
assert.compareArray(indicesMatch.indices[1], [1, 2]);
assert.compareArray(indicesMatch.indices[2], [2, 3]);
assert.compareArray(indicesMatch.indices.groups.name, [1, 2]);

// Dropping `d` from a copy removes the indices.
var noIndicesCopy = new RegExp(new RegExp("(b)", "d"), "");
assert.sameValue(noIndicesCopy.exec("abc").indices, undefined);

// Named capture groups survive a copy that shares the bytecode.
var namedCopy = new RegExp(new RegExp("(?<first>a)(?<second>b)", ""), "g");
var namedMatch = namedCopy.exec("xaby");
assert.sameValue(namedMatch.groups.first, "a");
assert.sameValue(namedMatch.groups.second, "b");
assert.sameValue("xaby".replace(namedCopy, "<$<second>$<first>>"), "x<ba>y");

// Duplicate named capture groups survive a copy as well.
var duplicateCopy = new RegExp(new RegExp("(?<x>a)|(?<x>b)", ""), "g");
assert.sameValue(duplicateCopy.exec("b").groups.x, "b");

// The escaped source of the copy round trips, and does not get escaped a second time.
var slashSource = new RegExp("a/b", "");
assert.sameValue(slashSource.source, "a\\/b");
assert.sameValue(new RegExp(slashSource, "g").source, "a\\/b");
assert.sameValue(new RegExp(slashSource, "i").source, "a\\/b");
assert.sameValue(new RegExp(new RegExp(slashSource, "g"), "i").source, "a\\/b");
assert.sameValue(new RegExp(slashSource, "g").test("xa/by"), true);

var emptySource = new RegExp("", "");
assert.sameValue(emptySource.source, "(?:)");
assert.sameValue(new RegExp(emptySource, "g").source, "(?:)");
assert.sameValue(new RegExp(emptySource, "u").source, "(?:)");

// An unescaped `[` in a character class is only valid outside unicode sets mode, so reusing the
// bytecode stays valid while adding `v` forces a reparse which rejects it
var unescapedBracket = new RegExp("[[]", "");
assert.sameValue(new RegExp(unescapedBracket, "g").source, "[[]");
assert.sameValue(new RegExp(unescapedBracket, "g").test("a[b"), true);
assert.sameValue(new RegExp(unescapedBracket, "dgy").test("["), true);
assert.sameValue(new RegExp(unescapedBracket, "u").test("a[b"), true, "`u` still accepts it");
assert.throws(SyntaxError, function () {
  new RegExp(unescapedBracket, "v");
});
assert.throws(SyntaxError, function () {
  new RegExp(new RegExp(unescapedBracket, "dgy"), "v");
});

// Copying with no flags argument at all shares the source regexp's flags too.
var noFlagsArgument = new RegExp(new RegExp("(a)b", "gi"));
assert.sameValue(noFlagsArgument.source, "(a)b");
assert.sameValue(noFlagsArgument.flags, "gi");
assert.compareArray(noFlagsArgument.exec("xABy"), ["AB", "A"]);

// An invalid flags string is still rejected, whichever branch would otherwise be taken.
assert.throws(SyntaxError, function () {
  new RegExp(new RegExp("a", "g"), "q");
});
assert.throws(SyntaxError, function () {
  new RegExp(new RegExp("a", "g"), "gg");
});
assert.throws(SyntaxError, function () {
  new RegExp(new RegExp("a", "g"), "uv");
});
