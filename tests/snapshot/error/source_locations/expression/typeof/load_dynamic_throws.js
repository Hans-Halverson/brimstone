Object.defineProperty(globalThis, 'x', {
  get() {
    throw new Error('Error on get');
  }
});

eval('typeof x');