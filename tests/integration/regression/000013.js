/*---
description: >
  Eval-like functions should not crash when the stack contains no source file, such as when executed
  as a promise reaction task.
---*/

Promise.resolve().then(Function);
Promise.resolve('a').then(eval);
Promise.resolve('a').then($262.evalScript);
