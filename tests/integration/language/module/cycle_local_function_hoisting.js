/*---
description: >
  Non-exported function declarations are instantiated before their module is evaluated, so they are
  callable from a module cycle.
flags: [module]
---*/

import { fnDuringCycle } from "./cycle_local_function_hoisting_FIXTURE.js";

function fn() { return "fn"; }

export function getFn() { return fn; }

// The function instantiated before evaluation must be the same object seen after evaluation
assert.sameValue(fnDuringCycle(), fn);
