/*---
description: GetNamedProperty cache does not cache module namespace objects.
flags: [module]
---*/

import * as ns from "./module_namespace_FIXTURE.js";
import { setValue } from "./module_namespace_FIXTURE.js";

// Module binding updates live, not affected by cache.
(function () {
  function get(o) { return o.value; }
  assert.sameValue(get(ns), 1);
  assert.sameValue(get(ns), 1);
  setValue(2);
  assert.sameValue(get(ns), 2);
})();

// A missing export reads as undefined.
(function () {
  function get(o) { return o.missing; }
  assert.sameValue(get(ns), undefined);
  assert.sameValue(get(ns), undefined);
})();
