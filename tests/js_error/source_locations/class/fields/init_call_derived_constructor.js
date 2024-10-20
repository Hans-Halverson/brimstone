class B {}

class C extends B {
  field = 1 + 2n;

  constructor() {
    // Field init call for derived constructor is placed at the super call expression
    super()
  }
}

new C();