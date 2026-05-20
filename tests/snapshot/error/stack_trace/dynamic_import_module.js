async function foo() {
  await import('./script_top_level_error.js');
}

await foo();