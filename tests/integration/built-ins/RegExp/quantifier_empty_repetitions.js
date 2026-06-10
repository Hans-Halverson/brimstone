/*---
description: Optional repetitions of a quantifier cannot match the empty string.
---*/

// Minimal repros
assert.compareArray(/(x*?)?/.exec('x'), ['x', 'x']);
assert.compareArray(/(x?|c)?/.exec('cc'), ['c', 'c']);

// Optional inlined repetitions without min
assert.compareArray(/(x?|c){0,3}/.exec('ccc'), ['ccc', 'c']);

// Optional loop repetitions without min
assert.compareArray(/(x?|c){0,20}/.exec('c'.repeat(20)), ['cccccccccccccccccccc','c']);

// Required inline repetitions followed by optional inline repetitions
assert.compareArray(/(x?|c){2,4}/.exec('cccc'), ['cc','c']);

// Required inline repetitions followed by optional loop repetitions
assert.compareArray(/(x?|c){1,15}/.exec('c'.repeat(15)), ['cccccccccccccc','c']);

// Required loop repetitions followed by optional inlined repetitions
assert.compareArray(/(x?|c){11,13}/.exec('c'.repeat(13)), ['cc', 'c']);

// Required loop repetitions followed by optional loop repetitions
assert.compareArray(/(x?|c){15,30}/.exec('c'.repeat(30)), ['ccccccccccccccc', 'c']);

// Required loop repetitions followed by unbounded optional repetitions
assert.compareArray(/(x?|c){2,}/.exec('c'.repeat(10)), ['cccccccccc', 'c']);

// Last non-empty repetition's captures are kept, even if followed by empty repetition
assert.compareArray(/(?:(a)+)*/.exec('a'), ['a', 'a']);
assert.compareArray(/(?:(a)+){2}/.exec('aa'), ['aa', 'a']);
assert.compareArray(/(?:b(a)+)*/.exec('ba'), ['ba', 'a']);
assert.compareArray(/(?:a?(?:(b*)|c)+)*/.exec('b'), ['b', 'b']);
assert.compareArray(/(?:(a)+)*\1/.exec('aa'), ['aa', 'a']);
