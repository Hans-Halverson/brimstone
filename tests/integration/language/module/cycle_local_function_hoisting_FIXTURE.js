import { getFn } from "./cycle_local_function_hoisting.js";

const fn = getFn();
assert.sameValue(fn(), "fn");

export function fnDuringCycle() {
  return fn;
}
