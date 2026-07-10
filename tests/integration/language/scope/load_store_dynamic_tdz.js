/*---
description: Dynamically loading and storing uninitialized lexical bindings.
---*/

// Dynamic load/store from own global scope
const readL1 = () => eval("l1");
const typeofL2 = () => eval("typeof l2");
const writeL3 = () => eval("l3 = 1");
const readC1 = () => eval("c1");
const typeofC2 = () => eval("typeof c2");
const writeC3 = () => eval("c3 = 2");
const readK1 = () => eval("K1");
const typeofK2 = () => eval("typeof K2");
const writeK3 = () => eval("K3 = 3");

// Dynamic load/store from a different script's global scope
const readL4 = Function('return eval("l4");');
const typeofL5 = Function('return eval("typeof l5");');
const writeL6 = Function('return eval("l6 = 4");');
const readC4 = Function('return eval("c4");');
const typeofC5 = Function('return eval("typeof c5");');
const writeC6 = Function('return eval("c6 = 5");');
const readK4 = Function('return eval("K4");');
const typeofK5 = Function('return eval("typeof K5");');
const writeK6 = Function('return eval("K6 = 6");');

assert.throws(ReferenceError, () => readL1());
assert.throws(ReferenceError, () => typeofL2());
assert.throws(ReferenceError, () => writeL3());
assert.throws(ReferenceError, () => readC1());
assert.throws(ReferenceError, () => typeofC2());
assert.throws(ReferenceError, () => writeC3());
assert.throws(ReferenceError, () => readK1());
assert.throws(ReferenceError, () => typeofK2());
assert.throws(ReferenceError, () => writeK3());
assert.throws(ReferenceError, () => readL4());
assert.throws(ReferenceError, () => typeofL5());
assert.throws(ReferenceError, () => writeL6());
assert.throws(ReferenceError, () => readC4());
assert.throws(ReferenceError, () => typeofC5());
assert.throws(ReferenceError, () => writeC6());
assert.throws(ReferenceError, () => readK4());
assert.throws(ReferenceError, () => typeofK5());
assert.throws(ReferenceError, () => writeK6());

let l1 = "l1";
let l2 = "l2";
let l3 = "l3";
let l4 = "l4";
let l5 = "l5";
let l6 = "l6";

const c1 = "c1";
const c2 = "c2";
const c3 = "c3";
const c4 = "c4";
const c5 = "c5";
const c6 = "c6";

class K1 {}
class K2 {}
class K3 {}
class K4 {}
class K5 {}
class K6 {}

assert.sameValue(readL1(), "l1");
assert.sameValue(typeofL2(), "string");
assert.sameValue(writeL3(), 1);
assert.sameValue(readC1(), "c1");
assert.sameValue(typeofC2(), "string");
assert.throws(TypeError, () => writeC3());
assert.sameValue(readK1(), K1);
assert.sameValue(typeofK2(), "function");
assert.sameValue(writeK3(), 3);
assert.sameValue(readL4(), "l4");
assert.sameValue(typeofL5(), "string");
assert.sameValue(writeL6(), 4);
assert.sameValue(readC4(), "c4");
assert.sameValue(typeofC5(), "string");
assert.throws(TypeError, () => writeC6());
assert.sameValue(readK4(), K4);
assert.sameValue(typeofK5(), "function");
assert.sameValue(writeK6(), 6);

// Dynamic load/store from function scope
(function test() {
  const readL1 = () => eval("l1");
  const typeofL2 = () => eval("typeof l2");
  const writeL3 = () => eval("l3 = 1");
  const readC1 = () => eval("c1");
  const typeofC2 = () => eval("typeof c2");
  const writeC3 = () => eval("c3 = 2");
  const readK1 = () => eval("K1");
  const typeofK2 = () => eval("typeof K2");
  const writeK3 = () => eval("K3 = 3");

  assert.throws(ReferenceError, () => readL1());
  assert.throws(ReferenceError, () => typeofL2());
  assert.throws(ReferenceError, () => writeL3());
  assert.throws(ReferenceError, () => readC1());
  assert.throws(ReferenceError, () => typeofC2());
  assert.throws(ReferenceError, () => writeC3());
  assert.throws(ReferenceError, () => readK1());
  assert.throws(ReferenceError, () => typeofK2());
  assert.throws(ReferenceError, () => writeK3());

  let l1 = "l1";
  let l2 = "l2";
  let l3 = "l3";
  const c1 = "c1";
  const c2 = "c2";
  const c3 = "c3";
  class K1 {}
  class K2 {}
  class K3 {}

  assert.sameValue(readL1(), "l1");
  assert.sameValue(typeofL2(), "string");
  assert.sameValue(writeL3(), 1);
  assert.sameValue(readC1(), "c1");
  assert.sameValue(typeofC2(), "string");
  assert.throws(TypeError, () => writeC3());
  assert.sameValue(readK1(), K1);
  assert.sameValue(typeofK2(), "function");
  assert.sameValue(writeK3(), 3);
})();
