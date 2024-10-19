Object.defineProperty(globalThis, 'x', {
  get() {
    throw new Error('Error on get');
  }
});

typeof x;