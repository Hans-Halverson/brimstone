/*---
description: Array.fromAsync behavior not covered by test262.
flags: [async]
---*/

let returnCalled = false;

const iter = {
  [Symbol.asyncIterator]() {
    return {
      next() { return Promise.resolve({ value: 1, done: false }); },
      return() { returnCalled = true; return Promise.resolve({ done: true }); },
    };
  },
};

const evil = Promise.resolve(42);
Object.defineProperty(evil, "constructor", {
  get() { throw new Error("boom"); },
});

Array.fromAsync(iter, () => evil)
  .then(
    () => {
      throw new Test262Error();
    },
    () => {
      assert(returnCalled);
    }
  )
  .then($DONE, $DONE);