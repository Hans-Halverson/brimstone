class B {
  get foo() {
    throw new Error();
  }
}

class C extends B {
  test() {
    super['fo' + 'o'] += 1;
  }
}

var c = new C();
c.test();