/*---
description: Loading and storing uninitialized global lexical bindings.
---*/

const readL1 = Function("return l1;");
const typeofL2 = Function("return typeof l2;");
const writeL3 = Function("return l3 = 1;");
const readC1 = Function("return c1;");
const typeofC2 = Function("return typeof c2;");
const writeC3 = Function("return c3 = 2;");
const readK1 = Function("return K1;");
const typeofK2 = Function("return typeof K2;");
const writeK3 = Function("return K3 = 3;");

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
