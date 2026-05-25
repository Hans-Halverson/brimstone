/*---
description: ArrayBuffer.prototype.slice clamps copying after resizeable buffer shrinks.
---*/

const buffer = new ArrayBuffer(8, { maxByteLength: 8 });
const bytes = new Uint8Array(buffer);

for (let i = 0; i < bytes.length; i++) {
  bytes[i] = i + 1;
}

const start = {
  valueOf() {
    buffer.resize(2);
    return 4;
  },
};

const sliced = buffer.slice(start);
const slicedBytes = new Uint8Array(sliced);

assert.sameValue(sliced.byteLength, 4);
assert.sameValue(slicedBytes[0], 0);
assert.sameValue(slicedBytes[1], 0);
assert.sameValue(slicedBytes[2], 0);
assert.sameValue(slicedBytes[3], 0);
