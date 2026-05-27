/*---
description: >
  Extra wide exception handler entries (bytecode size > 2^16). Previously were decoded incorrectly
  and could cause crashes.
---*/

let program = 'let result = 0;\n';
for (let i = 0; i < (2 << 11); i++) {
  program += `try { const x${i} = ${i}; } catch (e) {}\n`;
}
program += 'throw new Test262Error();';

assert.throws(Test262Error, () => eval(program));