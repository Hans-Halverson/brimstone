var obj = {
  get foo() {
    throw new Error();
  }
};

obj.foo++;