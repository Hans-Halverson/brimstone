/*---
description: >
  Test the RegExp's required literal filter in a variety of situations. Should be transparent but
  return correct results.
includes: [compareArray.js]
---*/

// Wrapping a pattern in a disjunction with an empty class, which never matches, leaves the language
// and the capture groups unchanged but leaves no literal that every alternative requires.
function withoutRequiredLiteral(source) {
  return "(?:" + source + "|[])";
}

// Every operation that runs the matcher, reporting the result it produced.
function runAll(source, flags, string) {
  return {
    exec: JSON.stringify(new RegExp(source, flags).exec(string)),
    test: new RegExp(source, flags).test(string),
    search: string.search(new RegExp(source, flags)),
    match: JSON.stringify(string.match(new RegExp(source, flags))),
    replace: string.replace(new RegExp(source, flags), "<$&>"),
    split: string.split(new RegExp(source, flags)),
  };
}

function assertSameResults(source, flags, string) {
  var message = "/" + source + "/" + flags + " on " + JSON.stringify(string);

  var filtered = runAll(source, flags, string);
  var unfiltered = runAll(withoutRequiredLiteral(source), flags, string);

  assert.sameValue(filtered.exec, unfiltered.exec, message + " exec");
  assert.sameValue(filtered.test, unfiltered.test, message + " test");
  assert.sameValue(filtered.search, unfiltered.search, message + " search");
  assert.sameValue(filtered.match, unfiltered.match, message + " match");
  assert.sameValue(filtered.replace, unfiltered.replace, message + " replace");
  assert.compareArray(filtered.split, unfiltered.split, message + " split");
}

var sources = [
  // A single run of literal code points
  "foobar",
  // Runs broken up by terms that require nothing, so only the longest run is required
  "a(?:bcdef)g",
  "[xy]abcde[xy]",
  // A run inside a term that always matches at least once
  "(abcd)+xy",
  // A run that is longer than the stored literal, so only a prefix of it is searched for
  "abcdefghijklmnopqrstuvwxyz",
  // Non-Latin1 code points interrupt a literal and a new literal starts after the interruption,
  // with the longest literal searched for. A full literal ignores later interruptions.
  "abcdefĀghijkl",
  "abcĀdefgh",
  "aĀĀbbbĀcccc",
  "x{2,5}abĀvwxyz",
  "Āabcdefgh.*xyz",
  "abcdefghijklmnopqrstĀuvwxyz",
  // Astral code points count as one code point in unicode mode and two surrogates otherwise
  "😀😀foobar",
  // Case insensitive mode searches only for stretches of code points without case variants
  "123456",
  "foo_123_bar",
  "é12345",
  "(?i:error404)xyz",
  // Runs that are only reachable on some paths, or not at the start of the match
  "(?:abcdef)?xy",
  "(?=abcdef)abc",
  "foobar|bazqux",
  // The shape of the pattern this filter was written for: a leading group that consumes at most one
  // code point, followed by a long required run.
  "(^|[^\\\\])\"\\\\/Date\\((-?[0-9]+)\\)",
  // Literals at a fixed distance into the match, which pins a match start to one position per
  // occurrence of the literal
  "x{3}foobar",
  "(?=xx)x{2}foobar",
  // Literals at a bounded range of distances, so several positions must be tried per occurrence
  "x{2,5}foobar",
  "(?:ab|cde)foobar",
  // Literals that may appear arbitrarily far into the match, which can only rule out an input
  "(\\w)\\1foobar",
  ".*foobar",
  // A line anchored pattern scans line starts while still using the literal to rule out inputs
  "^x*foobar",
  // Literals containing NUL code points, whose widened two byte form can overlap itself at a
  // misaligned offset in a two byte input
  "\\u0000\\u0000\\u0000",
  "\\u0000bc",
  "a\\u0000\\u0000\\u0000",
];

var flagCombinations = ["", "g", "i", "gi", "y", "gy", "u", "gu", "dgimsy"];

var strings = [
  "",
  // The literal present, absent, repeated, and in case variants
  "foobar",
  "xxfoobarxx",
  "foobarfoobar",
  "xxxxxxxxxx",
  "xxFooBarxx",
  "xx123456xx",
  // Inputs for the patterns that are not plain runs
  "xabcdex",
  "abcdabcdxy",
  "abcdefghijklmnopqrstuvwxyz",
  "abcdefghijklmnop",
  "abc",
  "xyz",
  "bazqux",
  // Inputs with line terminators for the line anchored pattern
  "zz\nxfoobar",
  "foobar\nzz",
  '{"d":"\\/Date(12345)\\/"}',
  // Inputs holding interrupted literals exactly and at surrounding offsets
  "abcdefĀghijkl",
  "abcĀdefgh",
  "xxabcĀdefghxx",
  "defgh",
  "aĀĀbbbĀcccc",
  "ccccxxxx",
  "xabĀvwxyz",
  "xxabĀvwxyz",
  "xxxxxabĀvwxyz",
  "xxxxxxabĀvwxyz",
  "abcdefghijklmnopqrstĀuvwxyz",
  "Āabcdefghxxxxyz",
  "\u0100abcdefghxyz",
  // Astral inputs around the required literal
  "😀😀foobar",
  "x😀😀foobar",
  "😀foobar",
  // Case variants of the stretch patterns
  "FOO_123_BAR",
  "foo_123_barxx",
  "foo_123_bar‰",
  "ERROR404xyz",
  "error404XYZ",
  "É12345",
  "é12345",
  // A two byte input holds the same code points, but is searched for the literal differently. The
  // trailing code point is what forces the string to be stored as two bytes.
  "xxfoobarxx‰",
  "xxxxxxxxxx‰",
  "xx123456xx‰",
  '{"d":"\\/Date(12345)\\/"}‰',
  // A lone surrogate cannot be part of the literal, but must not be matched into either
  "xxfoo\uD83Dbarxx",
  "xxfoobarxx\uD83D",
  // The literal at each distance around the bounds of the patterns above, so that a match starting
  // at exactly the earliest and latest viable position is covered, as well as just outside both.
  "xfoobar",
  "xxfoobar",
  "xxxfoobar",
  "xxxxfoobar",
  "xxxxxfoobar",
  "xxxxxxfoobar",
  "zzfoobarxxxfoobar",
  "abfoobar",
  "cdefoobar",
  "xxxfoobar‰",
  "xxxxxfoobar‰",
  // Two byte inputs where a misaligned occurrence of a NUL literal's widened form overlaps and
  // precedes the real aligned occurrence
  "a\u0000\u0000\u0000\u1234",
  "\u00ff\u0000\u0000\u0000\u1234",
  "\u1234a\u0000\u0000\u0000",
  "A\u6200\u6300\u0000bc",
  // One byte inputs holding the same NUL literals
  "a\u0000\u0000\u0000",
  "\u0000\u0000\u0000bc",
];

for (var i = 0; i < sources.length; i++) {
  for (var j = 0; j < flagCombinations.length; j++) {
    for (var k = 0; k < strings.length; k++) {
      assertSameResults(sources[i], flagCombinations[j], strings[k]);
    }
  }
}

// A last index past the required literal must not find a match behind it.
var afterLiteral = new RegExp("foobar", "g");
afterLiteral.lastIndex = 8;
assert.sameValue(afterLiteral.exec("foobar__"), null, "literal entirely before the last index");
assert.sameValue(afterLiteral.lastIndex, 0, "last index reset after failing past the literal");

var straddling = new RegExp("foobar", "g");
straddling.lastIndex = 2;
assert.sameValue(straddling.exec("__foobar__")[0], "foobar", "literal found after the last index");
assert.sameValue(straddling.lastIndex, 8, "last index after matching");

// A sticky match must respect the required literal window at a nonzero last index.
var stickyInWindow = new RegExp("x{2,5}foobar", "y");
stickyInWindow.lastIndex = 2;
assert.sameValue(stickyInWindow.exec("zzxxxfoobar")[0], "xxxfoobar", "sticky start within the literal window");
assert.sameValue(stickyInWindow.lastIndex, 11, "last index after sticky match");

var stickyOutsideWindow = new RegExp("x{2,5}foobar", "y");
stickyOutsideWindow.lastIndex = 4;
assert.sameValue(stickyOutsideWindow.exec("zzxxxfoobar"), null, "sticky start past the literal window");
assert.sameValue(stickyOutsideWindow.lastIndex, 0, "last index reset after sticky failure");
