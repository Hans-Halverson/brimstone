class B {
  get foo() {
    throw new Error();
  }
}

class C extends B {
  test() {
    super.foo
  }
}

var c = new C();
c.test();