/*---
description: Accessing `this` from eval within arrow functions.
---*/

const sentinel = {};
const sentinel2 = {};

// Arrow functions which inherit global `this`
assert.sameValue((() => eval('this'))(), this);
assert.sameValue((() => eval('this')).call(sentinel), this);
assert.sameValue((() => eval('this')).apply(sentinel, []), this);
assert.sameValue((() => (() => eval('this')).call(sentinel2)).call(sentinel), this);

// Arrow functions which inherit parent function's `this`
function outer1() {
    return (() => eval('this'))();
}
assert.sameValue(outer1.call(sentinel), sentinel);

function outer2() {
    return (() => eval('this')).call(sentinel);
}
assert.sameValue(outer2.call(sentinel2), sentinel2);

function outer3() {
    return (() => eval('this')).apply(sentinel, []);
}
assert.sameValue(outer3.call(sentinel2), sentinel2);

function outer4() {
    const nested = () => () => eval('this');
    return nested().call(sentinel);
}
assert.sameValue(outer4.call(sentinel2), sentinel2);

// Accessing `this` within nested eval
assert.sameValue((() => eval('eval("this")')).call(sentinel), this);

function outer5() {
    return (() => eval('eval("this")'))();
}
assert.sameValue(outer5.call(sentinel), sentinel);

// Indirect eval in an arrow function will return the global `this`
const indirectEval = eval;
assert.sameValue((() => indirectEval('this')).call(sentinel), this);

function outer6() {
    return (() => indirectEval('this')).call(sentinel);
}
assert.sameValue(outer6.call(sentinel2), this);

// Accessing `this` inside eval inside arrow function resolving to global `this`
assert.sameValue(eval("(() => this).call(sentinel)"), this);
assert.sameValue((() => eval("(() => this).call(sentinel)")).call(sentinel), this);

// Accessing `this` inside eval inside arrow function which inherits parent function's `this`
function outer7() {
  return eval("(() => this).call(sentinel2)");
};

assert.sameValue(outer7.call(sentinel), sentinel);


function outer8() {
  return (() => eval("(() => this)()")).call(sentinel2);
};

assert.sameValue(outer8.call(sentinel), sentinel);

// Eval inside arrow inside derived constructor before super() throws ReferenceError
class Base {}

class Derived1 extends Base {
  constructor() {
    (() => eval('this')).call(sentinel);
    super();
  }
}

assert.throws(ReferenceError, () => new Derived1());

// Eval inside arrow inside derived constructor after super() initializes `this`
class Derived2 extends Base {
  constructor() {
    super();
    this.saved = (() => eval('this')).call(sentinel);
  }
}

const derived2 = new Derived2();
assert.sameValue(derived2.saved, derived2);
