var obj = {
  set foo(_) {
    throw new Error();
  }
};

({ x: obj['fo' + 'o'] } = { x: 1 });