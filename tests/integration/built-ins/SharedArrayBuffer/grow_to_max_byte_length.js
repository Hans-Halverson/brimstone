/*---
description: Growing a SharedArrayBuffer to exactly its max byte length grows the buffer.
---*/

const sab = new SharedArrayBuffer(2, { maxByteLength: 4 });
const view = new Uint8Array(sab);
view[0] = 1;
view[1] = 2;

sab.grow(4);

assert.sameValue(sab.byteLength, 4);
assert.sameValue(sab.maxByteLength, 4);

// Existing contents are preserved and new bytes are zeroed
assert.sameValue(view.length, 4);
assert.sameValue(view[0], 1);
assert.sameValue(view[1], 2);
assert.sameValue(view[2], 0);
assert.sameValue(view[3], 0);
