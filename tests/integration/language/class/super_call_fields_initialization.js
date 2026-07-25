/*---
description: Fields are initialized at every super call site.
---*/

class Base {}

// Multiple super() sites in branches all initialize fields
class C1 extends Base {
  x = 1;
  constructor(cond) {
    if (cond) {
      super();
    } else {
      super();
    }
  }
}

assert.sameValue(new C1(true).x, 1);
assert.sameValue(new C1(false).x, 1);

// super() in an arrow function initializes fields
class C2 extends Base {
  x = 2;
  constructor() {
    const f = () => super();
    f();
  }
}

assert.sameValue(new C2().x, 2);

// super() in a nested arrow function initializes fields
class C3 extends Base {
  x = 3;
  constructor() {
    const f = () => () => super();
    f()();
  }
}

assert.sameValue(new C3().x, 3);

// super() in an async arrow function initializes fields synchronously
class C4 extends Base {
  x = 4;
  constructor() {
    const f = async () => super();
    f();
  }
}

assert.sameValue(new C4().x, 4);

// super() within direct eval initializes fields
class C5 extends Base {
  x = 5;
  constructor() {
    eval('super()');
  }
}

assert.sameValue(new C5().x, 5);

// super() in an arrow within direct eval initializes fields
class C6 extends Base {
  x = 6;
  constructor() {
    eval('(() => super())()');
  }
}

assert.sameValue(new C6().x, 6);

// Private fields, private methods, and computed fields are initialized at every super() site
const key = 'computed';
class C7 extends Base {
  [key] = 6;
  #priv = 7;

  #method() {
    return this.#priv;
  }
  constructor(cond) {
    if (cond) {
      super();
    } else {
      super();
    }
  }
  callPrivate() {
    return this.#method();
  }
}

assert.sameValue(new C7(true).computed, 6);
assert.sameValue(new C7(true).callPrivate(), 7);
assert.sameValue(new C7(false).computed, 6);
assert.sameValue(new C7(false).callPrivate(), 7);

// super() within a nested direct eval initializes fields
class C8 extends Base {
  x = 8;
  constructor() {
    eval('eval("super()")');
  }
}

assert.sameValue(new C8().x, 8);

// Classes without fields support super() in an arrow function
class C9 extends Base {
  constructor() {
    (() => super())();
  }
}

assert.sameValue(new C9() instanceof C9, true);

// Calling super() a second time from an arrow throws, and fields are only initialized once
let initCount = 0;
class C10 extends Base {
  x = ++initCount;
  constructor() {
    super();
    let threw = false;
    try {
      (() => super())();
    } catch (e) {
      threw = e instanceof ReferenceError;
    }
    this.threw = threw;
  }
}

const c10 = new C10();
assert.sameValue(c10.x, 1);
assert.sameValue(c10.threw, true);
assert.sameValue(initCount, 1);

// super() in a computed key of a nested class initializes the outer class's fields. The nested
// derived class's scope must not shadow the outer fields initializer.
class C11 extends Base {
  x = 11;
  constructor() {
    class Inner extends Base {
      [(super(), 'm')]() {}
    }
  }
}

assert.sameValue(new C11().x, 11);

// super() in a static computed key of a nested class initializes the outer class's fields
class C12 extends Base {
  x = 12;
  constructor() {
    class Inner extends Base {
      static [(super(), 'm')]() {}
    }
  }
}

assert.sameValue(new C12().x, 12);

// super() in the heritage clause of a nested class initializes the outer class's fields
class C13 extends Base {
  x = 13;
  constructor() {
    class Inner extends (super(), Base) {}
  }
}

assert.sameValue(new C13().x, 13);

// super() in an arrow created in a nested class's computed key and called after the nested class
// is defined initializes the outer class's fields, not the nested class's fields.
class C14 extends Base {
  x = 14;
  constructor() {
    let f;
    class Inner {
      [((f = () => super()), 'm')]() {}
      innerField = 999;
    }
    f();
  }
}

const c14 = new C14();
assert.sameValue(c14.x, 14);
assert.sameValue(Object.prototype.hasOwnProperty.call(c14, 'innerField'), false);

// Same as above with an outer class without fields: the nested class's fields must not be
// initialized on the outer instance.
class C15 extends Base {
  constructor() {
    let f;
    class Inner {
      [((f = () => super()), 'm')]() {}
      innerField = 999;
    }
    f();
  }
}

assert.sameValue(Object.prototype.hasOwnProperty.call(new C15(), 'innerField'), false);

// super() within direct eval in a nested class's computed key initializes the outer class's fields
class C16 extends Base {
  x = 16;
  constructor() {
    class Inner extends Base {
      [(eval('super()'), 'm')]() {}
    }
  }
}

assert.sameValue(new C16().x, 16);

// super() in the heritage clause of a nested class within direct eval initializes the outer
// class's fields
class C17 extends Base {
  x = 17;
  constructor() {
    eval("class Inner extends (super(), Base) {}");
  }
}

assert.sameValue(new C17().x, 17);
