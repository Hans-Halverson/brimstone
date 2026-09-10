/*---
description: Promise.try with no arguments returns a promise that rejects.
flags: [async]
---*/

Promise.try()
  .then(
    () => {
      throw new Test262Error("Promise should not resolve");
    },
    (error) => {
      assert(error instanceof TypeError);
    }
  )
  .then($DONE, $DONE);