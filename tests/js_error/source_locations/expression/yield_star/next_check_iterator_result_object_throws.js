var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return 1;
      }
    };
  }
}

function *generator() {
  yield* iterable;
}

let iter = generator();
iter.next();