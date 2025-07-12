/*---
description: Catch VM scope is properly exited after the catch block.
---*/

function outer() {
  var inOuterScope = 100;

  function inner() {
    try {
      // Does not throw so catch is not encountered
    } catch (thrown) {
      // Capture `thrown` to force catch VM scope to exist
      (() => thrown);
    }

    // Catch scope is properly exited, allowing for accessing outer scope at correct parent index
    assert.sameValue(inOuterScope, 100);
  }

  inner();
}

outer();