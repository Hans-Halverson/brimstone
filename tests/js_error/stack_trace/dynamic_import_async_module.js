async function foo() {
  await import('./top_level_await_module.js');
}

await foo();