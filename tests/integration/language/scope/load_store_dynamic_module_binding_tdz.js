/*---
description: Dynamically loading and storing uninitialized lexical module bindings.
flags: [module]
---*/

// The fixture module is evaluated first, dynamically accessing this module's exported bindings
// while they are still uninitialized.
import { checkAfterInitialization } from "./load_store_dynamic_module_binding_tdz_FIXTURE.js";

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

export let l1 = "l1";
export let l2 = "l2";
export let l3 = "l3";

export const c1 = "c1";
export const c2 = "c2";
export const c3 = "c3";

export class K1 {}
export class K2 {}
export class K3 {}

assert.sameValue(readL1(), "l1");
assert.sameValue(typeofL2(), "string");
assert.sameValue(writeL3(), 1);
assert.sameValue(readC1(), "c1");
assert.sameValue(typeofC2(), "string");
assert.throws(TypeError, () => writeC3());
assert.sameValue(readK1(), K1);
assert.sameValue(typeofK2(), "function");
assert.sameValue(writeK3(), 3);

checkAfterInitialization();
