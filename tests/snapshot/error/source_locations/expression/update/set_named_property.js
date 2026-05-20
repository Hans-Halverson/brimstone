var obj = {
  get foo() {
    return 1
  },
  set foo(_) {
    throw new Error();
  }
};

obj.foo = 1;