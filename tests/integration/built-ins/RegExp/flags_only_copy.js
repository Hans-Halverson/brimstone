/*---
description: >
  Constructing a RegExp from an existing RegExp plus a flags argument may reuse compiled bytecode.
includes: [compareArray.js]
---*/

// Flags that do not affect compilation, so the copy shares the source regexp's bytecode.
var runtimeOnlyFlags = ["", "d", "g", "y", "dg", "dy", "gy", "dgy"];

// Flags that do affect compilation, so the pattern must be reparsed and recompiled.
var compilationFlags = ["i", "m", "s", "u", "v", "im", "gis", "dgimsy"];

// The source is always taken from the source regexp, and the flags always from the argument,
// however the compiled regexp is obtained.
function assertCopy(source, sourceFlags, newFlags) {
  var original = new RegExp(source, sourceFlags);
  var copy = new RegExp(original, newFlags);
  var message = "/" + source + "/" + sourceFlags + " with " + JSON.stringify(newFlags);

  assert.sameValue(copy.source, original.source, message + " source");
  assert.sameValue(copy.flags, new RegExp(source, newFlags).flags, message + " flags");
  assert.sameValue(copy.lastIndex, 0, message + " last index");
  assert.notSameValue(copy, original, message + " is a new object");

  // Matching must agree with a regexp compiled from the same source and flags directly.
  var direct = new RegExp(original.source, newFlags);
  var strings = ["", "abc", "ABC", "abcabc", "a\nB", "aa/bb", "a\u{1F600}b"];
  for (var i = 0; i < strings.length; i++) {
    assert.sameValue(
      JSON.stringify(new RegExp(original, newFlags).exec(strings[i])),
      JSON.stringify(new RegExp(direct.source, newFlags).exec(strings[i])),
      message + " exec on " + JSON.stringify(strings[i])
    );
  }
}

var sources = ["abc", "(a)(b)", "(?<name>a)b", "a|b", "[a-c]+", "a\\/b", "\\d{2,3}", "^a.b$"];
var allFlags = runtimeOnlyFlags.concat(compilationFlags);

for (var i = 0; i < sources.length; i++) {
  for (var j = 0; j < allFlags.length; j++) {
    for (var k = 0; k < allFlags.length; k++) {
      assertCopy(sources[i], allFlags[j], allFlags[k]);
    }
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
