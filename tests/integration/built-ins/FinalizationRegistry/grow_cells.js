/*---
description: FinalizationRegistry cell array grows correctly in the presence of deleted cells.
flags: [async]
---*/

var called = [];
var registry = new FinalizationRegistry((heldValue) => called.push(heldValue));

// Must be large enough to force the cell array to grow well past its initial capacity.
var NUM_CELLS = 20;

// Number of cells that are deleted for each cell that is kept, so that deleted cells make up most
// of the cell array when it grows.
var NUM_DELETED_PER_CELL = 3;

(() => {
  // Keep every target alive until all registrations are done, otherwise a target may be collected
  // before its cell is unregistered.
  var targets = [];
  var unregisterToken = {};

  for (let i = 0; i < NUM_CELLS; i++) {
    // Register cells then immediately unregister them, leaving deleted cells behind. Deleted cells
    // must not be copied over by a grow, nor counted as occupied in the new cell array.
    for (let j = 0; j < NUM_DELETED_PER_CELL; j++) {
      let deletedTarget = {};
      targets.push(deletedTarget);
      registry.register(deletedTarget, "deleted", unregisterToken);
      registry.unregister(unregisterToken);
    }

    // Register a cell that is kept, so that the array always holds a mix of occupied and deleted
    // cells whenever it grows.
    let target = {};
    targets.push(target);
    registry.register(target, i);
  }
})();

// All targets are now unreachable so every cell that was not unregistered must have its finalizer
// callback run.
$262.gc();

Promise.resolve().then(() => {
  called.sort((a, b) => a - b);

  var expected = [];
  for (var i = 0; i < NUM_CELLS; i++) {
    expected.push(i);
  }

  assert.compareArray(called, expected);
}).then($DONE, $DONE);
