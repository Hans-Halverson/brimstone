var iter = {
  [Symbol.iterator]: function() {
    return {
      next() {
        throw new Error()
      }
    }
  }
}

async function foo() {
  // Error occurs in `next` and points to the `of`
  for await (var x of iter) {}
}

await foo();