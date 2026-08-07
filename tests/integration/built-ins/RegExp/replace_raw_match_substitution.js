/*---
description: >
  RegExp.prototype[@@replace] consumes the raw match of the builtin `exec` directly where possible.
---*/

// A RegExp with the same behaviour whose `exec` is a user-provided function, forcing @@replace down
// the path that reads a match result array instead of consuming the raw match.
var builtinExec = RegExp.prototype.exec;

function withWrappedExec(source, flags) {
  var regexp = new RegExp(source, flags);
  regexp.exec = function (string) {
    return builtinExec.call(this, string);
  };

  return regexp;
}

function assertSameReplacement(source, flags, string, replacement) {
  var message =
    "/" + source + "/" + flags +
    " on " + JSON.stringify(string) +
    " with " + JSON.stringify(replacement);

  assert.sameValue(
    string.replace(new RegExp(source, flags), replacement),
    string.replace(withWrappedExec(source, flags), replacement),
    message
  );
}

// Every substitution form, against patterns with and without named capture groups.
var replacements = [
  "-",
  "$$",
  "$&",
  "$`",
  "$'",
  "$`|$&|$'",
  "$1",
  "$2",
  "$3",
  "[$1][$2]",
  "$0",
  "$00",
  "$01",
  "$<name>",
  "$<missing>",
  "$<",
  "$",
  "x$",
  "$z",
  "a$1b$$c$&d",
];

var replacePatterns = [
  ["b", ""],
  ["b", "g"],
  ["(b)", "g"],
  ["(b)(c)", "g"],
  ["(z)?(b)", "g"],
  ["(?<name>b)", "g"],
  ["(?<name>b)(c)", "g"],
  ["(?<name>b)|(?<name>c)", "g"],
  ["", "g"],
  ["b|c", "g"],
];

for (var i = 0; i < replacePatterns.length; i++) {
  for (var j = 0; j < replacements.length; j++) {
    assertSameReplacement(replacePatterns[i][0], replacePatterns[i][1], "abcbd", replacements[j]);
    assertSameReplacement(replacePatterns[i][0], replacePatterns[i][1], "", replacements[j]);
    assertSameReplacement(replacePatterns[i][0], replacePatterns[i][1], "bbb", replacements[j]);
  }
}

// Only the captures a template references are materialized, so a template that references none must
// still produce the right result for a pattern with many captures.
var manyCaptures = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)";
assert.sameValue("abcdefghijkl".replace(new RegExp(manyCaptures), "-"), "-");
assert.sameValue("abcdefghijkl".replace(new RegExp(manyCaptures), "$1"), "a");
assert.sameValue("abcdefghijkl".replace(new RegExp(manyCaptures), "$12"), "l");
assert.sameValue("abcdefghijkl".replace(new RegExp(manyCaptures), "$1$12$5"), "ale");

// A two digit index that is out of range falls back to its first digit followed by a literal, so
// both the two digit capture and the single digit capture must be available to the template.
var tenCaptures = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)";
assert.sameValue("abcdefghij".replace(new RegExp(tenCaptures), "$10"), "j");
assert.sameValue("abcdefghij".replace(new RegExp(tenCaptures), "$11"), "a1");
assert.sameValue("abcdefghij".replace(new RegExp(tenCaptures), "$19"), "a9");
assert.sameValue("abcdefghij".replace(new RegExp(tenCaptures), "$99"), "i9");

// Both digits out of range leaves the whole reference as a literal.
assert.sameValue("ab".replace(new RegExp("(a)b"), "$99"), "$99");
assert.sameValue("ab".replace(new RegExp("(a)b"), "$29"), "$29");

var nineCaptures = "(a)(b)(c)(d)(e)(f)(g)(h)(i)";
assert.sameValue("abcdefghi".replace(new RegExp(nineCaptures), "$10"), "a0");
assert.sameValue("abcdefghi".replace(new RegExp(nineCaptures), "$9"), "i");

// A capture that did not participate in the match substitutes as the empty string.
assert.sameValue("abc".replace(new RegExp("(z)?(b)"), "[$1][$2]"), "a[][b]c");
assert.sameValue("abc".replace(new RegExp("(z)?(b)"), "$1"), "ac");

// A replacer function receives every capture, whether or not a template would reference it.
function replacerArguments(source, flags, string) {
  var seen = [];
  string.replace(new RegExp(source, flags), function () {
    seen.push(Array.prototype.slice.call(arguments));
    return "";
  });

  return seen;
}

assert.sameValue(
  JSON.stringify(replacerArguments("(z)?(b)(c)", "", "abcd")),
  JSON.stringify([[ "bc", undefined, "b", "c", 1, "abcd" ]]),
  "captures, position and string are passed to the replacer"
);

assert.sameValue(
  JSON.stringify(replacerArguments("(?<name>b)", "", "abc")),
  JSON.stringify([[ "b", "b", 1, "abc", { name: "b" } ]]),
  "a groups object is appended for a RegExp with named capture groups"
);

assert.sameValue(
  JSON.stringify(replacerArguments("b", "g", "abcb")),
  JSON.stringify([[ "b", 1, "abcb" ], [ "b", 3, "abcb" ]]),
  "no groups object is appended without named capture groups"
);

// Empty matches advance last index so a global replace terminates, and the replacement is inserted
// at every position.
assert.sameValue("abc".replace(new RegExp("", "g"), "-"), "-a-b-c-");
assert.sameValue("abc".replace(new RegExp("(?:)", "g"), "-"), "-a-b-c-");
assert.sameValue("".replace(new RegExp("", "g"), "-"), "-");
assert.sameValue("a\u{1F600}b".replace(new RegExp("", "gu"), "-"), "-a-\u{1F600}-b-");

// A sticky global replace only matches at consecutive positions from the start.
assert.sameValue("aab".replace(new RegExp("a", "gy"), "-"), "--b");
assert.sameValue("baa".replace(new RegExp("a", "gy"), "-"), "baa");

// The unchanged portions between and around matches are preserved.
assert.sameValue("xxbyybzz".replace(new RegExp("b", "g"), "<$&>"), "xx<b>yy<b>zz");
assert.sameValue("bxb".replace(new RegExp("b", "g"), ""), "x");
assert.sameValue("abc".replace(new RegExp("c$"), "<$&>"), "ab<c>");
assert.sameValue("abc".replace(new RegExp("^a"), "<$&>"), "<a>bc");

// A replacer function that returns a non-string has its result converted.
assert.sameValue(
  "abc".replace(new RegExp("b"), function () {
    return 42;
  }),
  "a42c"
);

// String.prototype.replaceAll goes through @@replace with a global RegExp.
assert.sameValue("abcb".replaceAll(new RegExp("(b)", "g"), "<$1>"), "a<b>c<b>");
assert.sameValue("abcb".replaceAll(new RegExp("(?<name>b)", "g"), "<$<name>>"), "a<b>c<b>");
