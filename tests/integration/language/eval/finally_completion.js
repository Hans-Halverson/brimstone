/*---
description: Completion values of try-catch-finally statements.
---*/

// Break or continue through a finally restores the completion value before the finally block.
assert.sameValue(eval("l: { try { 1; break l } finally { 2 } }"), 1);
assert.sameValue(eval("do { try { 1; continue } finally { 2 } } while (false)"), 1);
assert.sameValue(eval("l: { try { throw 0 } catch (e) { 1; break l } finally { 2 } }"), 1);
assert.sameValue(eval("l: { try { try { 1; break l } finally { 2 } } finally { 3 } }"), 1);
assert.sameValue(eval("l: { 1; try { break l } finally { 2 } }"), undefined);
assert.sameValue(eval("l: { try { 1; break l } finally { 2; break l } }"), 2);
