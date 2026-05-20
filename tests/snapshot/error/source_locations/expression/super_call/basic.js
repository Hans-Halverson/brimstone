class A {
  constructor() {
    throw new Error();
  }
}

class B extends A {
  constructor() {
    super();
  }
}

new B();