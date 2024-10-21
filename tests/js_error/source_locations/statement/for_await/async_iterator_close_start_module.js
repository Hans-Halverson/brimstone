var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return { value: 1, done: false };
      },
      get return() {
        throw new Error();
      }
    }
  }
};

async function foo() {
  for await (var x of iterable) {
    break;
  }
}

await foo();