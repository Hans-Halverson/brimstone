class C1 extends String {
  constructor() {
    // Simple super call, no arguments
    super()
  }
}

class C2 extends String {
  constructor() {
    // Super call with arguments
    var x = super(1, 2, 3)
    return x;
  }
}

class C3 extends String {
  constructor() {
    // Captured this and new.target
    super()

    (() => {
      this;
      new.target;
    });
  }
}

class C4 extends String {
  constructor() {
    // Super call in a nested function
    (() => super());

    // This is implicitly captured
    this.x;
  }
}