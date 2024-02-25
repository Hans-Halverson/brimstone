// OPTIONS: --run
try {
  eval(`
  "expression statement completions";
  1 + 2;
  a + b;
  "foo";
`)
} catch {}

eval(`
  "inner completions";
  var a = 1;
  if (a) {
    1;
  } else {
    2;
  }
`)
