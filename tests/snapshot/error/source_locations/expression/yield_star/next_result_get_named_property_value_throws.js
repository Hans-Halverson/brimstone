var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return {
          done: true,
          get value() {
            throw new Error();
          },
        };
      },
      get return() {
        throw new Error();
      }
    }
  }
};

function *generator() {
  yield* iterable;
}

var iter = generator();
iter.next();