var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        return { value: 1, done: false };
      },
      get return() {
        throw new Error();
      }
    }
  }
};

for (var x of iterable) {
  break;
}