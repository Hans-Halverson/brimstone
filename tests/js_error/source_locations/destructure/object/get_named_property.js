var obj = {
  get foo() {
    throw new Error();
  }
};

var { foo: x } = obj;