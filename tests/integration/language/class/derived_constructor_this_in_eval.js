
/*---
description: Accessing `this` from direct eval in a derived constructor.
---*/

class Base {
  overridden() {
    return 5;
  }

  method() {
    return 7;
  }
}

// Accessing `this` from direct eval without initialization errors
class C1 extends Base {
    constructor() {
      eval('this');
    }
}

assert.throws(ReferenceError, () => new C1());

// Successful access to `this` from direct eval after initialization
class C2 extends Base {
    constructor() {
      super();
      eval('this.fromEval = this.method();');
    }
}

const c2 = new C2();
assert.sameValue(c2.fromEval, 7);

// Super called in eval should work correctly and successfully initialize `this`
class C3 extends Base {
  constructor() {
    eval('super()');
  }
}

assert.sameValue(new C3().method(), 7);

// Super called twice within eval errors
class C4 extends Base {
  constructor() {
    super();
    eval('super()');
  }
}

assert.throws(ReferenceError, () => new C4());

// Super method calls should throw if `this` is uninitialized, even in eval
class C5 extends Base {
  constructor() {
    eval('super.method()');
  }
}

assert.throws(ReferenceError, () => new C5());

// Super method calls should work if `this` is initialized, even in eval
class C6 extends Base {
  overridden() {
    return 6;
  }

  constructor() {
    super();
    eval('this.fromEval = super.overridden();');
  }
}

const c6 = new C6();
assert.sameValue(c6.fromEval, 5);

// Super called in an arrow function within eval initializes `this`
class C7 extends Base {
  constructor() {
    eval('(() => super())()');
  }
}

assert.sameValue(new C7().method(), 7);

// Super called within a nested direct eval initializes `this`
class C8 extends Base {
  constructor() {
    eval('eval("super()")');
  }
}

assert.sameValue(new C8().method(), 7);

// Accessing `this` from an arrow function within eval without initialization errors
class C9 extends Base {
  constructor() {
    eval('(() => this)()');
  }
}

assert.throws(ReferenceError, () => new C9());

// Successful access to `this` from an arrow function within eval after initialization
class C10 extends Base {
  constructor() {
    eval('(() => super())()');
    this.fromEval = eval('(() => this.method())()');
  }
}

assert.sameValue(new C10().fromEval, 7);
