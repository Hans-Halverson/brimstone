/*---
description: Exported functions have the correct prototype.
flags: [module]
---*/

export function fn() { return 0; }
export async function asyncFn() { return 1; }
export function* genFn() { yield 2; }
export async function* asyncGenFn() { yield 3; }

const Function = (function() {}).constructor;
const AsyncFunction = (async function() {}).constructor;
const GeneratorFunction = (function*() {}).constructor;
const AsyncGeneratorFunction = (async function*() {}).constructor;

assert.sameValue(Object.getPrototypeOf(fn), Function.prototype);
assert.sameValue(Object.getPrototypeOf(asyncFn), AsyncFunction.prototype);
assert.sameValue(Object.getPrototypeOf(genFn), GeneratorFunction.prototype);
assert.sameValue(Object.getPrototypeOf(asyncGenFn), AsyncGeneratorFunction.prototype);
