import { readL, typeofL, writeL, readC, readK } from "./cycle_local_lexical_tdz.js";

assert.throws(ReferenceError, () => readL());
assert.throws(ReferenceError, () => typeofL());
assert.throws(ReferenceError, () => writeL());
assert.throws(ReferenceError, () => readC());
assert.throws(ReferenceError, () => readK());

export function checkAfterInitialization() {
  assert.sameValue(readL(), "l");
  assert.sameValue(typeofL(), "string");
  assert.sameValue(writeL(), "written");
  assert.sameValue(readC(), "c");
  assert.sameValue(typeof readK(), "function");
}
