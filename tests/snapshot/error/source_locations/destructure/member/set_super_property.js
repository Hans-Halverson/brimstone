class B {
  set foo(_) {
    throw new Error()
  }
}

class C extends B {
  test() {
    ({ x: super.foo } = { x: 1 });
  }
}

var c = new C();
c.test();