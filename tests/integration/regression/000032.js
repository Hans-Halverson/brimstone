/*---
description: Crash on dangling return address in callee's stack frame on primitive receiver.
---*/

Number.prototype.f = function () { return 1; };
function caller() { var s = 0; for (var i = 0; i < 20; i++) { s += (7).f(); } return s; }
if (caller() !== 20) throw new Error("bad");