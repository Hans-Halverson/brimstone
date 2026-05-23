var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return { value: 1, done: false };
      },
      throw() {
        throw new Error('throw');
      }
    };
  }
}

function *generator() {
  yield* iterable;
}

let iter = generator();
iter.next();
iter.throw();