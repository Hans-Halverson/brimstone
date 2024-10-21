var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return { value: 1, done: false };
      },
      return() {
        return {
          get value() {
            throw new Error();
          },
          done: true,
        }
      }
    };
  }
}

function *generator() {
  yield* iterable;
}

let iter = generator();
iter.next();
iter.return();