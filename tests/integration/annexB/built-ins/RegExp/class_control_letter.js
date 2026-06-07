/*---
description: Class control letter escapes in Annex B.
---*/

// Extra control letter escapes parsed in non-unicode mode
assert(new RegExp("[\\c0]").test("\x10"));
assert(new RegExp("[\\c1]").test("\x11"));
assert(new RegExp("[\\c2]").test("\x12"));
assert(new RegExp("[\\c3]").test("\x13"));
assert(new RegExp("[\\c4]").test("\x14"));
assert(new RegExp("[\\c5]").test("\x15"));
assert(new RegExp("[\\c6]").test("\x16"));
assert(new RegExp("[\\c7]").test("\x17"));
assert(new RegExp("[\\c8]").test("\x18"));
assert(new RegExp("[\\c9]").test("\x19"));
assert(new RegExp("[\\c_]").test("\x1F"));

// Extra control letter escapes not parsed in unicode mode
assert.throws(SyntaxError, () => new RegExp("[\\c0]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c1]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c2]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c3]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c4]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c5]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c6]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c7]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c8]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c9]", "u"));
assert.throws(SyntaxError, () => new RegExp("[\\c_]", "u"));

// Malformed control letter escape in unicode mode
assert.throws(SyntaxError, () => new RegExp("[\\c]", "u"));

// Malformed control letter escape doesn't throw in non-unicode mode
assert(new RegExp("^[\\c]*$").test("c\\\\"));

// Valid control letter escape in any mode
assert(new RegExp("[\\cA]").test("\x01"));
assert(new RegExp("[\\cZ]").test("\x1A"));
assert(new RegExp("[\\ca]").test("\x01"));
assert(new RegExp("[\\cz]").test("\x1A"));
assert(new RegExp("[\\cA]", "u").test("\x01"));
assert(new RegExp("[\\cZ]", "u").test("\x1A"));
assert(new RegExp("[\\ca]", "u").test("\x01"));
assert(new RegExp("[\\cz]", "u").test("\x1A"));