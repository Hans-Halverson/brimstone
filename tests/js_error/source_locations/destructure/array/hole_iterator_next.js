var numNextCalls = 0;

var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        if (numNextCalls >= 1) {
          throw new Error();
        }

        numNextCalls++;

        return { value: 1, done: false };
      }
    }
  }
}

var [a, , c] = iterable;