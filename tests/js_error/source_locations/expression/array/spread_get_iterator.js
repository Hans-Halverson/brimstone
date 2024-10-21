var iterable = {
  [Symbol.iterator]() {
    throw new Error();
  }
};

[...iterable];