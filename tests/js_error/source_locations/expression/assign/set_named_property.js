var obj = {
  set foo(_) {
    throw new Error();
  }
};

obj.foo = 1;