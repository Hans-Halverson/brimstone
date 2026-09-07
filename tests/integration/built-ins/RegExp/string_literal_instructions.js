/*---
description: >
  Test string literal instructions in RegExp bytecode across all relevant string representations
  and RegExp modes.
includes: [compareArray.js]
---*/

// Disable the required literal and match start filters so that the literal instruction is run.
function unfiltered(source, flags) {
  return new RegExp("(?:" + source + "|[])", flags);
}

function assertMatch(re, string, expectedIndex, expectedCaptures) {
  var message = String(re) + " on " + JSON.stringify(string);
  var result = re.exec(string);
  assert.notSameValue(result, null, message + " should match");
  assert.sameValue(result.index, expectedIndex, message + " index");
  assert.compareArray(result, expectedCaptures, message + " captures");
}

function assertNoMatch(re, string) {
  var message = String(re) + " on " + JSON.stringify(string);
  assert.sameValue(re.exec(string), null, message + " should not match");
}

// One byte literals against a one byte input, a two byte input in code unit mode, and a two byte
// input in code point mode.
assertMatch(/abc/, "xabcx", 1, ["abc"]);
assertMatch(/abc/, "xabcĀ", 1, ["abc"]);
assertMatch(/abc/u, "xabc\u{1F600}", 1, ["abc"]);

assertMatch(unfiltered("abc"), "ababc", 2, ["abc"]);
assertMatch(unfiltered("abc"), "ababcĀ", 2, ["abc"]);
assertMatch(unfiltered("abc", "u"), "ababc\u{1F600}", 2, ["abc"]);

assertNoMatch(unfiltered("abc"), "abd");
assertNoMatch(unfiltered("abc"), "ab");
assertNoMatch(unfiltered("abc"), "abdĀ");
assertNoMatch(unfiltered("abc"), "abĀ");
assertNoMatch(unfiltered("abc", "u"), "abd\u{1F600}");
assertNoMatch(unfiltered("abc", "u"), "ab\u{1F600}");

// Two byte literals can never match a one byte input, and are otherwise compared by code unit
assertMatch(/Āā|ab/, "xab", 1, ["ab"]);
assertNoMatch(unfiltered("Āā"), "ab");
assertMatch(/Āā/, "xĀāy", 1, ["Āā"]);
assertMatch(/Āā/u, "xĀāy", 1, ["Āā"]);
assertMatch(/abĀcd/, "xabĀcd", 1, ["abĀcd"]);
assertNoMatch(unfiltered("Āā"), "ĀĂ");
assertNoMatch(unfiltered("Āā"), "Ā");
assertNoMatch(unfiltered("Āā", "u"), "ĀĂ");
assertNoMatch(unfiltered("Āā", "u"), "Ā");

// Astral code points are stored as surrogate pairs and advance the input by two code units each
assertMatch(/\u{1F600}\u{1F600}/u, "x\u{1F600}\u{1F600}", 1, ["\u{1F600}\u{1F600}"]);
assertMatch(/\u{1F600}b/u, "x\u{1F600}b", 1, ["\u{1F600}b"]);
assertMatch(/😀/, "x\u{1F600}", 1, ["\u{1F600}"]);
assertNoMatch(unfiltered("\u{1F600}\u{1F600}", "u"), "\u{1F600}");

// Outside unicode mode surrogates are plain code units, so a literal may end inside a pair
assertMatch(/a\uD83D/, "a\u{1F600}", 0, ["a\uD83D"]);

// A run of a single code point is emitted as a code point literal rather than a string literal
assertMatch(/a.b/, "axb", 0, ["axb"]);
assertMatch(/a/, "ba", 1, ["a"]);

// Case insensitive matching splits runs at code points with case variants, keeping the rest as
// string literals that are compared exactly.
assertMatch(/ab12cd/i, "AB12CD", 0, ["AB12CD"]);
assertMatch(/ab12cd/i, "ab12cd", 0, ["ab12cd"]);
assertMatch(/a1b/i, "A1B", 0, ["A1B"]);
assertNoMatch(/ab12cd/i, "AB13CD");
assertMatch(/1-2_3/i, "1-2_3", 0, ["1-2_3"]);
assertMatch(/中文/i, "x中文", 1, ["中文"]);

// Long s and the Kelvin sign have case variants only in unicode mode, so they form a string literal
// outside unicode mode but need a case closure within it.
assertMatch(/ſſ/i, "ſſ", 0, ["ſſ"]);
assertNoMatch(/ſſ/i, "ss");
assertMatch(/ſſ/iu, "ss", 0, ["ss"]);
assertMatch(/ſſ/iu, "Sſ", 0, ["Sſ"]);
assertNoMatch(/KK/i, "kk");
assertMatch(/KK/iu, "kK", 0, ["kK"]);

// In unicode mode a lone surrogate in the pattern splits the run so that two adjacent lone
// surrogates never match a surrogate pair in the input, while a pair escaped as two code units is
// a single astral code point that joins the run.
assertMatch(/ab\u{D83D}cd/u, "ab\uD83Dcd", 0, ["ab\uD83Dcd"]);
assertNoMatch(/ab\u{D83D}\u{DE00}cd/u, "ab\u{1F600}cd");
assertNoMatch(/ab\u{D83D}\u{DE00}cd/iu, "ab\u{1F600}cd");
assertMatch(/ab\uD83D\uDE00cd/u, "ab\u{1F600}cd", 0, ["ab\u{1F600}cd"]);
assertMatch(/ab😀cd/u, "ab\u{1F600}cd", 0, ["ab\u{1F600}cd"]);

// Outside unicode mode surrogates do not split the run
assertMatch(/ab\uD83Dcd/, "ab\uD83Dcd", 0, ["ab\uD83Dcd"]);

// Lookbehinds match literals backwards, reversing the order of the parts of a split run, and fail
// when fewer code units precede the position than the literal needs.
assertMatch(/(?<=abc)d/, "abcd", 3, ["d"]);
assertNoMatch(unfiltered("(?<=abc)d"), "bcd");
assertNoMatch(unfiltered("(?<=abc)d"), "xbcd");
assertMatch(/(?<=abc)Ā/, "abcĀ", 3, ["Ā"]);
assertMatch(/(?<=Āā)x/, "Āāx", 2, ["x"]);
assertMatch(/(?<=12a34)x/i, "12A34x", 5, ["x"]);
assertMatch(/(?<=ab\u{D83D}cd)x/u, "ab\uD83Dcdx", 5, ["x"]);
assertMatch(/(?<=\u{1F600}\u{1F600})x/u, "\u{1F600}\u{1F600}x", 4, ["x"]);
assertMatch(/(?<=\u{1F600}\u{1F600})x/u, "\u{1F600}\u{1F600}\u{1F600}x", 6, ["x"]);
assertNoMatch(unfiltered("(?<=\u{1F600}\u{1F600})x", "u"), "y\u{1F600}x");

// Identical literals share one constant, and one byte and two byte constants of any lengths may be
// mixed in a single pattern.
assertMatch(/abc.*abc/, "abcxabc", 0, ["abcxabc"]);
assertMatch(/Āā.Āā/, "ĀāxĀā", 0, ["ĀāxĀā"]);
assertMatch(/ab.cd.ef/, "abxcdyef", 0, ["abxcdyef"]);
assertMatch(/abc.Āā/, "abcxĀā", 0, ["abcxĀā"]);
assertMatch(/Āā.abc.ĂăĄ/, "ĀāxabcyĂăĄ", 0, ["ĀāxabcyĂăĄ"]);
