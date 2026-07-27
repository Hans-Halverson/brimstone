/*---
description: >
  Loading `this` before super() in a derived constructor should not write the uninitialized empty
  value to the destination register before throwing the ReferenceError.
---*/

class Uncaptured extends Object {
  constructor() {
    let t = 1;
    try {
      t = this;
    } catch {}
    assert.sameValue(t, 1);
    super();
  }
}
new Uncaptured();

class Captured extends Object {
  constructor() {
    let t = 1;
    try {
      t = this;
    } catch {}
    assert.sameValue(t, 1);
    super();
    // Force `this` to be captured
    (() => this)();
  }
}
new Captured();

class InEval extends Object {
  constructor() {
    eval(`
      let t = 1;
      try {
        t = this;
      } catch {}
      assert.sameValue(t, 1);
    `);
    super();
  }
}
new InEval();
