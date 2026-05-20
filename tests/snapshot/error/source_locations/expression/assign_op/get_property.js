var obj = {
  get foo() {
    throw new Error();
  }
};

obj['fo' + 'o'] += 1;