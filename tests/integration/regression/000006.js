/*---
description: Bug triggering an assertion failure in bitwise operations
---*/

// Throw error on the second call to toPrimitive
let i = 0;
let threw = false

const object = ({ [Symbol.toPrimitive]() {
  if (i++ == 1) {
    threw = true
    throw new Error();
  } else {
    return 2;
  }
} });

try {
  1 >> object;
  1 >> object;

  assert(false);
} catch {
  assert(threw);
}