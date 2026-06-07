/*---
description: Quantifier matches should only be cleared if the match was empty.
---*/

assert.sameValue(/^(x)?\1y$/.exec("xy"), null);
assert.compareArray(/^(x)?\1y$/.exec("y"), ["y", undefined]);