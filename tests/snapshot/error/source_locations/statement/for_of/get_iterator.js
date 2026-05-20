var iterable = {
  [Symbol.iterator]() {
    throw new Error();
  }
}

for (var x of iterable) {}