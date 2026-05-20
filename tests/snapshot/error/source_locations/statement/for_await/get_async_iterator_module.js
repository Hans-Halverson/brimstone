var iterable = {
  [Symbol.iterator]: function() {
    throw new Error();
  }
}

async function foo() {
  for await (var x of iterable) {}
}

await foo();