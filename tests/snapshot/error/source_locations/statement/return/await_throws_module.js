let promise = new Promise(() => {});
Object.defineProperty(promise, 'constructor', { get() { throw new Error(); } });

async function *generator() {
  return promise;
}

let gen = generator();
await gen.next();