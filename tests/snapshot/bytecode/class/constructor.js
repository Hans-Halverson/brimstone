class ExplicitConstructor {
  constructor(p) {
    return p;
  }
}

class BaseDefaultConstructor {}

class ThisInBaseConstructor {
  constructor() {
    this.x;
    this.x = 1;
  }
}

class ThisInDerivedConstructor extends String {
  constructor() {
    super();
    this.x;
    this.x = 1;
  }
}

class DerivedConstructorWithReturnValue extends String {
  constructor() {
    super();
    return 1;
  }
}

class DerivedConstructorWithNoArgReturn extends String {
  constructor() {
    super();
    return;
  }
}

class CapturedThisInDerivedConstructor extends String {
  constructor() {
    super();
    (() => {
      this;
    });

    this.x;
    this.x = 1;
  }
}

class CapturedThisInDerivedConstructorWithReturnValue extends String {
  constructor() {
    super();
    (() => {
      this;
    });

    return 1;
  }
}

class CapturedThisInDerivedConstructorWithNoArgReturn extends String {
  constructor() {
    super();
    (() => {
      this;
    });

    return;
  }
}

class FinallyWithDerivedReturn extends String {
  constructor() {
    super();

    try {
      1;
      return;
    } finally {
      2;
    }
  }
}

class FinallyWithDerivedReturnCapturedThis extends String {
  constructor() {
    super();

    (() => this);

    try {
      1;
      return;
    } finally {
      2;
    }
  }
}