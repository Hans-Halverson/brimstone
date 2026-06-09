/*---
description: Quantifier matches are cleared at the start of every iteration.
---*/

assert.compareArray(/(?:\1(a)){2,}/.exec("aa"), ["aa", "a"]);
assert.compareArray(/((\3|b)\2(a)){2,}/.exec("bbaababbabaaaaabbaaaabba"), ["bbaa", "a", "", "a"]);

