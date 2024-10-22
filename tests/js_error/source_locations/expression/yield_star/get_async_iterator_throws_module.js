var iterable = {
  [Symbol.iterator]() {
    throw new Error();
  }
}

async function *generator() {
  yield* iterable;
}

let iter = generator();
await iter.next();