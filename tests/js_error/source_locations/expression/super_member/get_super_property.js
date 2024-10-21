class B {
  get foo() {
    throw new Error();
  }
}

class C extends B {
  test() {
    super['fo' + 'o']
  }
}

var c = new C();
c.test();