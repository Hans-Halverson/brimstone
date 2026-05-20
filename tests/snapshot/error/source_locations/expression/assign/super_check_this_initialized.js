class B {
  foo = 1;
}

class C extends B {
  constructor() {
    super.foo = 2;
  }
}

new C();