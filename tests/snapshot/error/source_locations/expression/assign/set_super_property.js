class B {
  set foo(_) {
    throw new Error()
  }
}

class C extends B {
  test() {
    super.foo = 2;
  }
}

var c = new C();
c.test();