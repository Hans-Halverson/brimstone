/*---
description: >
  Non-exported lexical bindings are in the TDZ when accessed before their module is evaluated due to
  a module cycle.
flags: [module]
---*/

import { checkAfterInitialization } from "./cycle_local_lexical_tdz_FIXTURE.js";

let l = "l";
const c = "c";
class K {}

export function readL() { return l; }
export function typeofL() { return typeof l; }
export function writeL() { l = "written"; return l; }
export function readC() { return c; }
export function readK() { return K; }

checkAfterInitialization();
