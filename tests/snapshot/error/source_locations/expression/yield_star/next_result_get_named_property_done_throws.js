var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return {
          value: 1,
          get done() {
            throw new Error();
          }
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