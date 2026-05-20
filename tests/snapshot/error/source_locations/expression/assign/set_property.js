var obj = {
  set foo(_) {
    throw new Error();
  }
};

obj['fo' + 'o'] = 1;