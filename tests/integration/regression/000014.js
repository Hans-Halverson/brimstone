/*---
description: Crash in AsyncGeneratorAwaitReturn rejection handler
---*/

async function* f() {}
f().return(Promise.reject());