function named() {
  class C {
    named1 = 2;
    static named2 = 3;
  }
}

function computed() {
  class C {
    [1 + 2] = 2;
    static [3 + 4] = 4;
  }
}

function private() {
  class C {
    #private1 = 1;
    static #private2 = 2;
  }
}

function noInitializer() {
  class C {
    named1;
    [1 + 2];
    #private1;
    static named2;
    static [3 + 4];
    static #private2;
  }
}

function withConstructor() {
  class C {
    named = 1;
    [1 + 2] = 2;

    constructor() {
      var x = 1;
      () => x;
    }
  }
}

function withImplicitDerivedConstructor() {
  class C extends String {
    named = 1;
  }
}

function withDerivedConstructor() {
  class C extends String {
    named = 1;
    constructor() {
      2;
      super();
      3;
    }
  }
}

function withMethods() {
  class C {
    namedField = 1;
    namedMethod() {}
    [1 + 2] = 2;
    [3 + 4]() {}
    #private1 = 3;
    #private2() {}
  }
}

function withCapturedName() {
  class C {
    [1 + 2] = 2;
    #private1() {};

    method() {
      return C;
    }
  }
}

function withStaticInitializers() {
  class C {
    static [1 + 2] = 2;
    static {
      3;
    }
    static #private = 4;
    static {
      5;
    }
  }
}

function namedEvaluation() {
  class C {
    named1 = () => {};
    static named2 = () => {};

    [1 + 2] = () => {};
    static [3 + 4] = () => {};

    #private1 = () => {};
    static #private2 = () => {};
  }
}

function superMemberInFields() {
  class C {
    field1 = super.foo;
    static field2 = super.foo;
  }
}

function thisInFields() {
  class C {
    field1 = this;
    static field2 = this;
  }
}

function newTargetInFields() {
  class C {
    field1 = new.target;
    static field2 = new.target;
  }
}

function fieldNoInitializer() {
  class C {
    field1;
    static field2;
  }
}

function privateFieldNoInitializer() {
  class C {
    #field1;
    static #field2;
  }
}