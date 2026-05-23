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