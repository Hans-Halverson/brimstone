let promise = new Promise(() => {});
Object.defineProperty(promise, 'constructor', { get() { throw new Error(); } });

let iter = {
  [Symbol.asyncIterator]() {
    return {
      next() {
        return promise
      }
    };
  }
}

async function *generator() {
  yield* iter;
}

let gen = generator();
await gen.next();