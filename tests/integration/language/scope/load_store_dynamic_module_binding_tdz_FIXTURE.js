// This module is evaluated before the module that imports it, so all of the imported bindings are
// still uninitialized while the top level of this module is evaluated.
import { l1, l2, l3, c1, c2, K1, K2 } from "./load_store_dynamic_module_binding_tdz.js";

const readL1 = () => eval("l1");
const typeofL2 = () => eval("typeof l2");
const writeL3 = () => eval("l3 = 1");
const readC1 = () => eval("c1");
const typeofC2 = () => eval("typeof c2");
const readK1 = () => eval("K1");
const typeofK2 = () => eval("typeof K2");

assert.throws(ReferenceError, () => readL1());
assert.throws(ReferenceError, () => typeofL2());
assert.throws(TypeError, () => writeL3());
assert.throws(ReferenceError, () => readC1());
assert.throws(ReferenceError, () => typeofC2());
assert.throws(ReferenceError, () => readK1());
assert.throws(ReferenceError, () => typeofK2());

export function checkAfterInitialization() {
  assert.sameValue(readL1(), "l1");
  assert.sameValue(typeofL2(), "string");
  assert.throws(TypeError, () => writeL3());
  assert.sameValue(readC1(), "c1");
  assert.sameValue(typeofC2(), "string");
  assert.sameValue(readK1(), K1);
  assert.sameValue(typeofK2(), "function");
}
