/*---
description: JSON.parse enforces the ECMA-404 number grammar.
---*/

// Leading plus sign is never allowed.
assert.throws(SyntaxError, () => JSON.parse('+1'));
assert.throws(SyntaxError, () => JSON.parse('+0'));
assert.throws(SyntaxError, () => JSON.parse('+1.5'));
assert.throws(SyntaxError, () => JSON.parse('+.5'));
assert.throws(SyntaxError, () => JSON.parse('+0.5'));
assert.throws(SyntaxError, () => JSON.parse('+10'));

// Leading zeros are never allowed.
assert.throws(SyntaxError, () => JSON.parse('01'));
assert.throws(SyntaxError, () => JSON.parse('00'));
assert.throws(SyntaxError, () => JSON.parse('000'));
assert.throws(SyntaxError, () => JSON.parse('09'));
assert.throws(SyntaxError, () => JSON.parse('012'));
assert.throws(SyntaxError, () => JSON.parse('0123'));
assert.throws(SyntaxError, () => JSON.parse('01e5'));
assert.throws(SyntaxError, () => JSON.parse('01.5'));
assert.throws(SyntaxError, () => JSON.parse('-01'));
assert.throws(SyntaxError, () => JSON.parse('-00'));
assert.throws(SyntaxError, () => JSON.parse('-0123'));

// Lone signs and dangling fractions/exponents are never allowed.
assert.throws(SyntaxError, () => JSON.parse('-'));
assert.throws(SyntaxError, () => JSON.parse('+'));
assert.throws(SyntaxError, () => JSON.parse('.'));
assert.throws(SyntaxError, () => JSON.parse('-e5'));
assert.throws(SyntaxError, () => JSON.parse('-x'));
assert.throws(SyntaxError, () => JSON.parse('.5'));
assert.throws(SyntaxError, () => JSON.parse('-.'));
assert.throws(SyntaxError, () => JSON.parse('-.5'));
assert.throws(SyntaxError, () => JSON.parse('-.0'));
assert.throws(SyntaxError, () => JSON.parse('1.'));
assert.throws(SyntaxError, () => JSON.parse('12.'));
assert.throws(SyntaxError, () => JSON.parse('-0.'));
assert.throws(SyntaxError, () => JSON.parse('1.e5'));
assert.throws(SyntaxError, () => JSON.parse('.e5'));
assert.throws(SyntaxError, () => JSON.parse('e5'));
assert.throws(SyntaxError, () => JSON.parse('1e'));
assert.throws(SyntaxError, () => JSON.parse('1E'));
assert.throws(SyntaxError, () => JSON.parse('1e+'));
assert.throws(SyntaxError, () => JSON.parse('1e-'));
assert.throws(SyntaxError, () => JSON.parse('1e.5'));

// Repeated signs, dots, or exponents are never allowed.
assert.throws(SyntaxError, () => JSON.parse('--1'));
assert.throws(SyntaxError, () => JSON.parse('++1'));
assert.throws(SyntaxError, () => JSON.parse('1-'));
assert.throws(SyntaxError, () => JSON.parse('1.2.3'));
assert.throws(SyntaxError, () => JSON.parse('1e5e5'));
assert.throws(SyntaxError, () => JSON.parse('1e5.5'));

// Non-decimal numbers and numeric separators are not allowed in JSON.
assert.throws(SyntaxError, () => JSON.parse('0x1'));
assert.throws(SyntaxError, () => JSON.parse('0X1F'));
assert.throws(SyntaxError, () => JSON.parse('0o7'));
assert.throws(SyntaxError, () => JSON.parse('0b1'));
assert.throws(SyntaxError, () => JSON.parse('1_000'));
assert.throws(SyntaxError, () => JSON.parse('1n'));

// Infinity and NaN are not allowed in JSON.
assert.throws(SyntaxError, () => JSON.parse('Infinity'));
assert.throws(SyntaxError, () => JSON.parse('-Infinity'));
assert.throws(SyntaxError, () => JSON.parse('+Infinity'));
assert.throws(SyntaxError, () => JSON.parse('NaN'));
assert.throws(SyntaxError, () => JSON.parse('-NaN'));

// Internal whitespace inside a number, and trailing tokens, are never allowed.
assert.throws(SyntaxError, () => JSON.parse('- 1'));
assert.throws(SyntaxError, () => JSON.parse('1 2'));
assert.throws(SyntaxError, () => JSON.parse('1 .5'));
assert.throws(SyntaxError, () => JSON.parse('1. 5'));

// Valid integer part formats.
assert.sameValue(JSON.parse('0'), 0);
assert.sameValue(JSON.parse('-0'), -0);
assert.sameValue(JSON.parse('1'), 1);
assert.sameValue(JSON.parse('-1'), -1);
assert.sameValue(JSON.parse('123'), 123);
assert.sameValue(JSON.parse('-123'), -123);
assert.sameValue(JSON.parse('10'), 10);
assert.sameValue(JSON.parse('1000000'), 1000000);

// Valid fraction formats.
assert.sameValue(JSON.parse('0.5'), 0.5);
assert.sameValue(JSON.parse('-0.5'), -0.5);
assert.sameValue(JSON.parse('0.0'), 0);
assert.sameValue(JSON.parse('-0.0'), -0);
assert.sameValue(JSON.parse('1.0'), 1);
assert.sameValue(JSON.parse('1.50'), 1.5);
assert.sameValue(JSON.parse('100.001'), 100.001);
assert.sameValue(JSON.parse('0.000001'), 0.000001);

// Valid exponent formats.
assert.sameValue(JSON.parse('1e5'), 100000);
assert.sameValue(JSON.parse('1E5'), 100000);
assert.sameValue(JSON.parse('1e+5'), 100000);
assert.sameValue(JSON.parse('1E+5'), 100000);
assert.sameValue(JSON.parse('1e-5'), 0.00001);
assert.sameValue(JSON.parse('1E-5'), 0.00001);
assert.sameValue(JSON.parse('1e0'), 1);
assert.sameValue(JSON.parse('1e00'), 1);
assert.sameValue(JSON.parse('1e05'), 100000);
assert.sameValue(JSON.parse('1e007'), 10000000);
assert.sameValue(JSON.parse('0e5'), 0);
assert.sameValue(JSON.parse('0e0'), 0);
assert.sameValue(JSON.parse('1.5e3'), 1500);
assert.sameValue(JSON.parse('-1.5e3'), -1500);
assert.sameValue(JSON.parse('1.5E3'), 1500);

// Exponent magnitudes that overflow.
assert.sameValue(JSON.parse('1e308'), 1e308);
assert.sameValue(JSON.parse('1e400'), Infinity);
assert.sameValue(JSON.parse('-1e400'), -Infinity);
assert.sameValue(JSON.parse('1e-400'), 0);

// Leading and trailing whitespace is allowed.
assert.sameValue(JSON.parse('  1'), 1);
assert.sameValue(JSON.parse('1  '), 1);
assert.sameValue(JSON.parse('\t\n 1.5 \n'), 1.5);
assert.sameValue(JSON.parse(' -1e5 '), -100000);

// Invalid whitespace code points.
assert.sameValue(JSON.parse(' \t\r\n5 \t\r\n'), 5);
assert.throws(SyntaxError, () => JSON.parse('\f1'));
assert.throws(SyntaxError, () => JSON.parse('\v1'));
assert.throws(SyntaxError, () => JSON.parse('\u00A01'));
assert.throws(SyntaxError, () => JSON.parse('\uFEFF1'));
assert.throws(SyntaxError, () => JSON.parse('\u20281'));
assert.throws(SyntaxError, () => JSON.parse('1\f'));
