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

eval(`
  "if completion";
  if (true) {
    1
  }
`)

eval(`
  "switch completion";
  switch (true) {}
`)


eval(`
  "while completion";
  while (false) { 1; }
`)

eval(`
  "do-while completion";
  do { 1; } while (false);
`)

eval(`
  "with completion";
  with ({}) { 1; }
`)

eval(`
  "for completion";
  for (; false; ) { 1; }
`)

eval(`
  "for-in completion";
  for (var x in {}) { 1; }
`)

eval(`
  "for-of completion";
  for (var x of []) { 1; }
`)

eval(`
  "try-catch completion";
  try { 1; } catch { 2; }
`)

eval(`
  "try-finally completion";
  try { 1; } finally { 2; }
`)

eval(`
  "try-catch-finally completion";
  try { 1; } catch { 2; } finally { 3; }
`)
