/*---
description: RegExp.prototype.source escaping
---*/

// Empty pattern escapes to an anonymous capture group
assert.sameValue(new RegExp("").source, "(?:)");

// No escaping needed
assert.sameValue(new RegExp("(?:)").source, "(?:)");
assert.sameValue(new RegExp("abc").source, "abc");
assert.sameValue(new RegExp("^a.b*c+d?$").source, "^a.b*c+d?$");
assert.sameValue(new RegExp("(a|b)\\d{2,3}").source, "(a|b)\\d{2,3}");

// Unescaped forward slash outside class is escaped
assert.sameValue(new RegExp("/").source, "\\/");
assert.sameValue(new RegExp("a/b/c").source, "a\\/b\\/c");

// Already escaped forward slash is unchanged
assert.sameValue(new RegExp("\\/").source, "\\/");

// Forward slash inside a character class is unchanged
assert.sameValue(new RegExp("[/]").source, "[/]");
assert.sameValue(new RegExp("[\\/]").source, "[\\/]");
assert.sameValue(new RegExp("[/]/").source, "[/]\\/");
assert.sameValue(new RegExp("[a]/[b]").source, "[a]\\/[b]");

// Escaped brackets do not start or end a class
assert.sameValue(new RegExp("\\[/").source, "\\[\\/");
assert.sameValue(new RegExp("\\]/").source, "\\]\\/");
assert.sameValue(new RegExp("[\\]/]").source, "[\\]/]");
assert.sameValue(new RegExp("[[/]").source, "[[/]");

// Line terminators are always escaped, whether literal or escaped
assert.sameValue(new RegExp("\n").source, "\\n");
assert.sameValue(new RegExp("\\n").source, "\\n");
assert.sameValue(new RegExp("\r").source, "\\r");
assert.sameValue(new RegExp("\\r").source, "\\r");
assert.sameValue(new RegExp(" ").source, "\\u2028");
assert.sameValue(new RegExp(" ").source, "\\u2029");
assert.sameValue(new RegExp("a\nb").source, "a\\nb");
assert.sameValue(new RegExp("[\n]").source, "[\\n]");