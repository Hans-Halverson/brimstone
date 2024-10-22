let promise = new Promise(() => {});
Object.defineProperty(promise, 'constructor', { get() { throw new Error('Error on get'); } });

await promise;