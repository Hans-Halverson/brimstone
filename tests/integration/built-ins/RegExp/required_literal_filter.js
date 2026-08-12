/*---
description: >
  Test the RegExp's required literal filter in a variety of situations. Should be transparent but
  return correct results.
---*/

// Wrapping a pattern in a disjunction with an empty class, which never matches, leaves the language
// and the capture groups unchanged but leaves no literal that every alternative requires.
function withoutRequiredLiteral(source) {
  return "(?:" + source + "|[])";
}

// Compare two exec results: both null, or the same captures and match position. Named groups are
// derived from the captures, so equal captures imply equal groups.
function sameMatch(a, b) {
  if (a === null || b === null) return a === b;
  if (a.length !== b.length || a.index !== b.index) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

function sameArray(a, b) {
  if (a.length !== b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

// The failure message is only built on an actual mismatch, off the hot path of passing combos.
function fail(source, flags, string, op, filtered, unfiltered) {
  var message = "/" + source + "/" + flags + " on " + JSON.stringify(string) + " " + op;
  assert.sameValue(JSON.stringify(filtered), JSON.stringify(unfiltered), message);
  throw new Error(message + ": results differ outside their JSON form");
}

// Every operation that runs the matcher must report the same result through the filtered and the
// unfiltered regexp. The regexps are reused across operations and strings, so reset lastIndex
// before each operation to make them behave like freshly constructed ones.
function assertSameResults(filteredRe, unfilteredRe, source, flags, string) {
  filteredRe.lastIndex = 0;
  unfilteredRe.lastIndex = 0;
  var filteredExec = filteredRe.exec(string);
  var unfilteredExec = unfilteredRe.exec(string);
  if (!sameMatch(filteredExec, unfilteredExec)) {
    fail(source, flags, string, "exec", filteredExec, unfilteredExec);
  }

  filteredRe.lastIndex = 0;
  unfilteredRe.lastIndex = 0;
  var filteredTest = filteredRe.test(string);
  var unfilteredTest = unfilteredRe.test(string);
  if (filteredTest !== unfilteredTest) {
    fail(source, flags, string, "test", filteredTest, unfilteredTest);
  }

  filteredRe.lastIndex = 0;
  unfilteredRe.lastIndex = 0;
  var filteredSearch = string.search(filteredRe);
  var unfilteredSearch = string.search(unfilteredRe);
  if (filteredSearch !== unfilteredSearch) {
    fail(source, flags, string, "search", filteredSearch, unfilteredSearch);
  }

  filteredRe.lastIndex = 0;
  unfilteredRe.lastIndex = 0;
  var filteredMatch = string.match(filteredRe);
  var unfilteredMatch = string.match(unfilteredRe);
  if (!sameMatch(filteredMatch, unfilteredMatch)) {
    fail(source, flags, string, "match", filteredMatch, unfilteredMatch);
  }

  filteredRe.lastIndex = 0;
  unfilteredRe.lastIndex = 0;
  var filteredReplace = string.replace(filteredRe, "<$&>");
  var unfilteredReplace = string.replace(unfilteredRe, "<$&>");
  if (filteredReplace !== unfilteredReplace) {
    fail(source, flags, string, "replace", filteredReplace, unfilteredReplace);
  }

  filteredRe.lastIndex = 0;
  unfilteredRe.lastIndex = 0;
  var filteredSplit = string.split(filteredRe);
  var unfilteredSplit = string.split(unfilteredRe);
  if (!sameArray(filteredSplit, unfilteredSplit)) {
    fail(source, flags, string, "split", filteredSplit, unfilteredSplit);
  }
}

var flagCombinations = ["", "g", "i", "gi", "y", "gy", "u", "gu", "dgimsy"];

// Strings run against every pattern under every flag combination: the literal absent from an
// empty, one byte, two byte, and lone surrogate input, plus inputs holding the literal that most
// of the patterns share. The trailing ‰ is what forces a string to be stored as two bytes.
//
// Half of these hold the literal rather than merely being rejected, and a two byte buffer is
// searched for the literal by a different path than a one byte buffer, so these must not be
// narrowed to a subset of the flag combinations. They are what guarantees that every pattern
// exercises the two byte search under each flag combination, since most of the per case strings
// below are one byte only.
var commonStrings = [
  "",
  "xxxxxxxxxx",
  "xxfoobarxx‰",
  "xxxxxxxxxx‰",
  "xxfoo\uD83Dbarxx",
  "xxfoobarxx\uD83D",
];

// The literal at each distance around the bounds of the fixed and bounded distance patterns below,
// so that a match starting at exactly the earliest and latest viable position is covered, as well
// as just outside both.
var literalDistanceStrings = [
  "xfoobar",
  "xxfoobar",
  "xxxfoobar",
  "xxxxfoobar",
  "xxxxxfoobar",
  "xxxxxxfoobar",
  "zzfoobarxxxfoobar",
  "xxxfoobar‰",
  "xxxxxfoobar‰",
];

// Two byte inputs where a misaligned occurrence of a NUL literal's widened form overlaps and
// precedes the real aligned occurrence, plus one byte inputs holding the same NUL literals
var nulStrings = [
  "a\u0000\u0000\u0000\u1234",
  "\u00ff\u0000\u0000\u0000\u1234",
  "\u1234a\u0000\u0000\u0000",
  "A\u6200\u6300\u0000bc",
  "a\u0000\u0000\u0000",
  "\u0000\u0000\u0000bc",
];

// Each pattern is paired with the inputs written for it: the literal present, absent, repeated, in
// case variants, and at surrounding offsets. Every pattern also runs against the common strings
// above. Running the full cross product of patterns and strings instead only added rejections and
// matches already covered by each pattern's own inputs, such as the same literal found at a
// further offset in a longer run of filler.
var cases = [
  // A single run of literal code points
  { source: "foobar", strings: ["foobar", "xxfoobarxx", "foobarfoobar", "xxFooBarxx", "foobar\nzz"] },
  // Runs broken up by terms that require nothing, so only the longest run is required
  {
    source: "a(?:bcdef)g",
    // The last input is two byte, since none of the common strings match this pattern.
    strings: ["abcdefg", "abcdefghijklmnop", "abc", "xabcdex", "abcdefghijklmnopqrstĀuvwxyz"],
  },
  { source: "[xy]abcde[xy]", strings: ["xabcdex", "abc", "xyz"] },
  // A run inside a term that always matches at least once
  { source: "(abcd)+xy", strings: ["abcdabcdxy", "abc", "xyz"] },
  // A run that is longer than the stored literal, so only a prefix of it is searched for
  { source: "abcdefghijklmnopqrstuvwxyz", strings: ["abcdefghijklmnopqrstuvwxyz", "abcdefghijklmnop"] },
  // Non-Latin1 code points interrupt a literal and a new literal starts after the interruption,
  // with the longest literal searched for. A full literal ignores later interruptions.
  { source: "abcdefĀghijkl", strings: ["abcdefĀghijkl", "abcdefghijklmnop", "defgh"] },
  { source: "abcĀdefgh", strings: ["abcĀdefgh", "xxabcĀdefghxx", "defgh", "abc"] },
  { source: "aĀĀbbbĀcccc", strings: ["aĀĀbbbĀcccc", "ccccxxxx"] },
  {
    source: "x{2,5}abĀvwxyz",
    strings: ["xabĀvwxyz", "xxabĀvwxyz", "xxxxxabĀvwxyz", "xxxxxxabĀvwxyz"],
  },
  { source: "Āabcdefgh.*xyz", strings: ["Āabcdefghxxxxyz", "Āabcdefghxyz"] },
  { source: "abcdefghijklmnopqrstĀuvwxyz", strings: ["abcdefghijklmnopqrstĀuvwxyz"] },
  // Astral code points count as one code point in unicode mode and two surrogates otherwise
  { source: "😀😀foobar", strings: ["😀😀foobar", "x😀😀foobar", "😀foobar"] },
  // Case insensitive mode searches only for stretches of code points without case variants
  { source: "123456", strings: ["xx123456xx", "xx123456xx‰"] },
  { source: "foo_123_bar", strings: ["FOO_123_BAR", "foo_123_barxx", "foo_123_bar‰"] },
  { source: "é12345", strings: ["É12345", "é12345"] },
  { source: "(?i:error404)xyz", strings: ["ERROR404xyz", "error404XYZ"] },
  // Runs that are only reachable on some paths, or not at the start of the match
  // The last input of each is two byte, since none of the common strings match these patterns.
  {
    source: "(?:abcdef)?xy",
    strings: ["abcdabcdxy", "xyz", "abcdefghijklmnop", "Āabcdefghxxxxyz"],
  },
  { source: "(?=abcdef)abc", strings: ["abcdefghijklmnop", "abc", "abcdefĀghijkl"] },
  { source: "foobar|bazqux", strings: ["bazqux", "xxfoobarxx", "xyz"] },
  // The shape of the pattern this filter was written for: a leading group that consumes at most one
  // code point, followed by a long required run.
  {
    source: "(^|[^\\\\])\"\\\\/Date\\((-?[0-9]+)\\)",
    strings: ['{"d":"\\/Date(12345)\\/"}', '{"d":"\\/Date(12345)\\/"}‰'],
  },
  // Literals at a fixed distance into the match, which pins a match start to one position per
  // occurrence of the literal
  { source: "x{3}foobar", strings: literalDistanceStrings },
  { source: "(?=xx)x{2}foobar", strings: literalDistanceStrings },
  // Literals at a bounded range of distances, so several positions must be tried per occurrence
  { source: "x{2,5}foobar", strings: literalDistanceStrings },
  {
    source: "(?:ab|cde)foobar",
    strings: ["abfoobar", "cdefoobar", "xfoobar", "xxfoobar", "xxxfoobar"],
  },
  // Literals that may appear arbitrarily far into the match, which can only rule out an input
  { source: "(\\w)\\1foobar", strings: ["xxfoobarxx", "zzfoobarxxxfoobar", "xfoobar"] },
  { source: ".*foobar", strings: ["xxfoobarxx", "zz\nxfoobar", "foobar\nzz"] },
  // A line anchored pattern scans line starts while still using the literal to rule out inputs
  { source: "^x*foobar", strings: ["zz\nxfoobar", "foobar\nzz", "xxxfoobar"] },
  // Literals containing NUL code points, whose widened two byte form can overlap itself at a
  // misaligned offset in a two byte input
  { source: "\\u0000\\u0000\\u0000", strings: nulStrings },
  { source: "\\u0000bc", strings: nulStrings },
  { source: "a\\u0000\\u0000\\u0000", strings: nulStrings },
];


for (var i = 0; i < cases.length; i++) {
  var source = cases[i].source;
  var strings = commonStrings.concat(cases[i].strings);
  for (var j = 0; j < flagCombinations.length; j++) {
    var flags = flagCombinations[j];
    var filteredRe = new RegExp(source, flags);
    var unfilteredRe = new RegExp(withoutRequiredLiteral(source), flags);
    for (var k = 0; k < strings.length; k++) {
      assertSameResults(filteredRe, unfilteredRe, source, flags, strings[k]);
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
