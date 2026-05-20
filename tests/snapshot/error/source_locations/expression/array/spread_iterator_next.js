var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        throw new Error();
      }
    }
  }
};

[...iterable]