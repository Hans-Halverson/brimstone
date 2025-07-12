/*---
description: Block VM scope is properly exited when block ends in abrupt completion.
---*/

function outer() {
  var inOuterScope = 100;

  function inner() {
    label: {
      // Capture to force VM scope to exist
      let captured = 1;
      (() => captured);

      // Abrupt completion at end of block
      break label;
    }

    // Block scope is properly cleaned up, allowing for accessing outer scope
    assert.sameValue(inOuterScope, 100);
  }

  inner();
}

outer();