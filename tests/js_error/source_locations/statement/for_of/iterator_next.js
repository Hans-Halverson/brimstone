var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        throw new Error();
      }
    }
  }
};

for (var x of iterable) {}