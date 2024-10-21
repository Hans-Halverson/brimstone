class B {}

class C extends B {
  constructor() {
    super();
    super();
  }
}

new C();