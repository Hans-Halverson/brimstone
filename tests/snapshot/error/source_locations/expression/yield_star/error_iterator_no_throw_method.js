var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return { value: 1, done: false };
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