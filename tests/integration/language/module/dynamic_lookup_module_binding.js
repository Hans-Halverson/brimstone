/*---
description: Dynamic lookup and stores of module bindings, i.e. exported bindings.
flags: [module]
---*/

export let x = 1;
export let y = 2;

// Dynamic lookup of module bindings
eval('assert.sameValue(x, 1)');

// Dynamic store to module bindings
assert.sameValue(y, 2);
eval('y = 3');
assert.sameValue(y, 3);