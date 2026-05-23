var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        throw new Error('next');
      }
    };
  }
}

function *generator() {
  yield* iterable;
}

let iter = generator();
iter.next();