/*---
description: >
  Assigning to a local binding clobbered the binding's previous value if an error was thrown during
  evaluation.
---*/

// Class expression whose static block throws
function classStaticBlock() {
  let a = 1;
  try {
    a = class {
      static {
        throw 0;
      }
    };
  } catch (e) {
    assert.sameValue(e, 0);
  }
  return a;
}
assert.sameValue(classStaticBlock(), 1);

// Class expression whose static field initializer throws
function classStaticField() {
  var a = 1;
  try {
    a = class {
      static x = (() => {
        throw 0;
      })();
    };
  } catch (e) {
    assert.sameValue(e, 0);
  }
  return a;
}
assert.sameValue(classStaticField(), 1);

// Second super call throws after this is already initialized
class Base {}
class SecondSuper extends Base {
  constructor() {
    let a = 1;
    super();
    try {
      a = super();
    } catch (e) {
      assert(e instanceof ReferenceError);
    }
    this.a = a;
  }
}
assert.sameValue(new SecondSuper().a, 1);

// Field initializer throws during the first super call
class ThrowingField extends Base {
  x = (() => {
    throw 0;
  })();
  constructor() {
    let a = 1;
    try {
      a = super();
    } catch (e) {
      assert.sameValue(e, 0);
    }
    this.a = a;
  }
}
assert.sameValue(new ThrowingField().a, 1);

// Object rest where a getter throws
function restGetter() {
  let a = 1;
  try {
    ({ ...a } = {
      get x() {
        throw 0;
      },
    });
  } catch (e) {
    assert.sameValue(e, 0);
  }
  return a;
}
assert.sameValue(restGetter(), 1);

// Object rest in a var declaration, which assigns to the hoisted binding
function restVarDecl() {
  var a = 1;
  try {
    var { ...a } = {
      get x() {
        throw 0;
      },
    };
  } catch (e) {
    assert.sameValue(e, 0);
  }
  return a;
}
assert.sameValue(restVarDecl(), 1);
