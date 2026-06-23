/*---
description: Function names which are only disallowed in strict mode.
flags: [noStrict]
---*/

// Disallowed immediately in strict mode
assert.throws(SyntaxError, () => eval("'use strict'; function implements() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function interface() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function let() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function package() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function private() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function protected() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function public() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function static() {}"));
assert.throws(SyntaxError, () => eval("'use strict'; function yield() {}"));

// Rejected when re-parsing function name after parsing body
assert.throws(SyntaxError, () => eval("function implements() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function interface() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function let() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function package() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function private() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function protected() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function public() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function static() { 'use strict'; }"));
assert.throws(SyntaxError, () => eval("function yield() { 'use strict'; }"));

// Applies to named function expressions
assert.throws(SyntaxError, () => eval("(function implements() { 'use strict'; })"));
