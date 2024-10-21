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

var [] = iterable;