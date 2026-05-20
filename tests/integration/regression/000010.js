/*---
description: Crashed when an already GC'd WeakRef was GC'd again.
---*/

const ref = new WeakRef({});

$262.gc();
assert.sameValue(ref.deref(), undefined);

$262.gc();
assert.sameValue(ref.deref(), undefined);
