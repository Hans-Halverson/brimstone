var o = {
  foo() {
    throw new Error();
  }
};

o.foo`test`;