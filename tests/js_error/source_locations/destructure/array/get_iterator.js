var iterable = {
  [Symbol.iterator]() {
    throw new Error();
  }
};

var [] = iterable;