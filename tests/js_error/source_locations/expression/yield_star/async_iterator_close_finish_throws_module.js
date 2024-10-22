var iterable = {
  [Symbol.asyncIterator]() {
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

async function *generator() {
  yield* iterable;
}

var iter = generator();
await iter.next();
await iter.throw();