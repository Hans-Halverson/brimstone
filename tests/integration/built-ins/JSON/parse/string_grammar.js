/*---
description: JSON.parse enforces the ECMA-404 string grammar.
---*/

// Simple unescaped code points.
assert.sameValue(JSON.parse('"one two"'), 'one two');
assert.sameValue(JSON.parse('"one⬤two"'), 'one⬤two');

// Control characters are not allowed unescaped.
for (let c = 0; c <= 0x1F; c++) {
  const src = '"' + String.fromCharCode(c) + '"';
  assert.throws(SyntaxError, () => JSON.parse(src));
}

// Unterminated strings.
assert.throws(SyntaxError, () => JSON.parse('"abc'));
assert.throws(SyntaxError, () => JSON.parse('"'));
assert.throws(SyntaxError, () => JSON.parse('"\\"'));

// Single quotes are not allowed as string delimiters.
assert.throws(SyntaxError, () => JSON.parse("'abc'"));

// Simple escape sequences.
assert.sameValue(JSON.parse('"\\""'), '"');
assert.sameValue(JSON.parse('"\\\\"'), '\\');
assert.sameValue(JSON.parse('"\\/"'), '/');
assert.sameValue(JSON.parse('"\\b"'), '\b');
assert.sameValue(JSON.parse('"\\f"'), '\f');
assert.sameValue(JSON.parse('"\\n"'), '\n');
assert.sameValue(JSON.parse('"\\r"'), '\r');
assert.sameValue(JSON.parse('"\\t"'), '\t');
assert.sameValue(JSON.parse('"\\b\\f\\n\\r\\t"'), '\b\f\n\r\t');

// Invalid escape sequences.
assert.throws(SyntaxError, () => JSON.parse('"\\x41"'));
assert.throws(SyntaxError, () => JSON.parse('"\\a"'));
assert.throws(SyntaxError, () => JSON.parse('"\\0"'));
assert.throws(SyntaxError, () => JSON.parse('"\\v"'));

// Unicode escapes.
assert.sameValue(JSON.parse('"\\u0041"'), 'A');
assert.sameValue(JSON.parse('"\\u00e9"'), 'é');
assert.sameValue(JSON.parse('"\\u0020"'), ' ');
assert.sameValue(JSON.parse('"\\u007F"'), String.fromCharCode(0x7F));
assert.sameValue(JSON.parse('"\\u25cBCD"'), '○CD');

// Invalid unicode escape syntax.
assert.throws(SyntaxError, () => JSON.parse('"\\u"'));
assert.throws(SyntaxError, () => JSON.parse('"\\u0"'));
assert.throws(SyntaxError, () => JSON.parse('"\\u00"'));
assert.throws(SyntaxError, () => JSON.parse('"\\u004"'));
assert.throws(SyntaxError, () => JSON.parse('"\\uXYZW"'));
assert.throws(SyntaxError, () => JSON.parse('"\\u123G"'));
assert.throws(SyntaxError, () => JSON.parse('"\\u{41}"'));

// Control characters are allowed when escaped.
assert.sameValue(JSON.parse('"\\u0000"'), String.fromCharCode(0));
assert.sameValue(JSON.parse('"\\u0000"').charCodeAt(0), 0);
assert.sameValue(JSON.parse('"\\u0009"'), '\t');
assert.sameValue(JSON.parse('"\\u001F"'), String.fromCharCode(0x1F));
assert.sameValue(JSON.parse('"\\u001f"'), String.fromCharCode(0x1F));

// Surrogate pairs and lone surrogates.
assert.sameValue(JSON.parse('"\\uD83D\\uDE00"'), '😀');
assert.sameValue(JSON.parse('"\\uD834"'), '\uD834');
assert.sameValue(JSON.parse('"\\uDD1E"'), '\uDD1E');
assert.sameValue(JSON.parse('"\\uD834x"'), '\uD834x');
