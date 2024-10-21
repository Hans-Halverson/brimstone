var iter = {
  [Symbol.iterator]: function() {
    return {
      next() {
        return 1;
      }
    }
  }
}

async function foo() {
  for await (var x of iter) {}
}

await foo();