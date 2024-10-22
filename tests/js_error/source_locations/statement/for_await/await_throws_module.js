let promise = new Promise(() => {});
Object.defineProperty(promise, 'constructor', { get() { throw new Error(); } });

var iterable = {
  [Symbol.asyncIterator]() {
    return {
      next() {
        return promise;
      }
    }
  }
};

async function foo() {
  for await (var x of iterable) {}
}

await foo();