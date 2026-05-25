/*---
description: Strings that exceed the length limit should throw a RangeError instead of crashing.
---*/

function concatStringTooLarge() {
  const str = "a".repeat(2 ** 16);
  let sum = 0;
  for (let i = 0; i < 2 ** 16; i++) {
      sum += str;
  }
}

assert.throws(RangeError, concatStringTooLarge);

function repeatStringTooLarge() {
  ("a".repeat(2 ** 16)).repeat((2 ** 31));
}

assert.throws(RangeError, repeatStringTooLarge);