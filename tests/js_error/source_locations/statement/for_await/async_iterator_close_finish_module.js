var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return { value: 1, done: false };
      },
      return() {
        return 1;
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