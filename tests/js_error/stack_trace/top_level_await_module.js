async function foo() {
  throw new Error('test');
}

await foo();