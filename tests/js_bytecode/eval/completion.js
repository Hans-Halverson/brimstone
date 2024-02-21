// OPTIONS: --run
eval(`
  "expression statement completions";
  1 + 2;
  a + b;
  "foo";
`)

eval(`
  "inner completions";
  var a = 1;
  if (a) {
    1;
  } else {
    2;
  }
`)
