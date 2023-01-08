class A {}

class A extends B {}

(class A {});

(class A extends B {});

(class {});

(class extends B {});

class C {
  ;;;
}

// Static initializer blocks

class C {
  static {}

  static {
    let i = 0;
    b;
  }
}