class B {
  foo = 1;
}

class C extends B {
  constructor() {
    ({ x: super.foo } = { x: 1 });
  }
}

var c = new C();