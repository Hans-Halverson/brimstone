var iterable = {
  [Symbol.asyncIterator]() {
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

async function *generator() {
  yield* iterable;
}

var iter = generator();
await iter.next();
await iter.throw();