class A {}

class A2 extends B {}

(class A {});

(class A extends B {});

(class {});

(class extends B {});

class C {
  ;;;
}

// Static initializer blocks

class C2 {
  static {}

  static {
    let i = 0;
    b;
  }
}