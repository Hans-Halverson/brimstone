var iterable = {
  [Symbol.asyncIterator]() {
    return {
      next() {
        return {
          done: false,
          get value() {
            throw new Error();
          },
        };
      }
    }
  }
};

async function *generator() {
  yield* iterable;
}

var iter = generator();
await iter.next();