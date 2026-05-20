class B {
  get foo() {
    return 1;
  }

  set foo(_) {
    throw new Error()
  }
}

class C extends B {
  test() {
    super.foo++;
  }
}

var c = new C();
c.test();