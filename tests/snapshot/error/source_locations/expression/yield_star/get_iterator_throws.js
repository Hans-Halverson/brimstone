var iterable = {
  [Symbol.iterator]() {
    throw new Error();
  }
}

function *generator() {
  yield* iterable;
}

let iter = generator();
iter.next();