/*---
description: >
  Ensure the RegExp comparison register is cleared after a failed lookaround so it does not leak to
  affect another match.
---*/

// Code point set checks could leak the comparison register
assert.sameValue(/(?![^z])z[^z]/.exec("zc")[0], "zc");
assert.sameValue(/(?![^z])z[a-c]/.exec("zq"), null);

// Word boundary assertions could leak the comparison register
assert.sameValue(/a(?!\b)[^q]/.exec("ab")[0], "ab");
assert.sameValue(/a(?!\B)[^q]/.exec("a.")[0], "a.");
