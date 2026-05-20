class B {
  foo = 1;
}

class C extends B {
  constructor() {
    super.foo++;
  }
}

new C();