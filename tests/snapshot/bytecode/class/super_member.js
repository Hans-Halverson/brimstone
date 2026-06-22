class C1 extends String {
  constructor() {
    super();
    super.x;
  }

  instanceSuperMember() {
    return super.x;
  }

  static staticSuperMember() {
    return super.x;
  }
}

class C2 extends String {
  constructor() {
    () => super();

    // Captured `this`
    super.x;
  }
}

class C3 extends Object {
  method() {
    // Super references parent class
    class Inner extends (super.foo) {}
  }
}

class C4 extends Object {
  method() {
    // Super references parent class
    class Inner extends (() => super.foo)() {}
  }
}

class C5 extends Object {
  static method() {
    // Super references parent class
    class Inner extends (super.foo) {}
  }
}

const obj = {
  method() {
    // Super references parent object
    class Inner extends (super.foo) {}
  }
};