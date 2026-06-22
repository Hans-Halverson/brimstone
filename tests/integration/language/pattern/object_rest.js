/*---
description: Tests for object destructuring with a rest element.
---*/

// ToPropertyKey on a computed property is called exactly once
(function ComputedSingleToPropertyKey() {
  let count = 0;
  const probe = {
    toString() {
      count++;
      return "f";
    }
  }

  const { [probe]: x, ...y } = {};

  assert.sameValue(count, 1);  
})();
