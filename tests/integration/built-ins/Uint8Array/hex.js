/*---
description: Uint8Array hex conversion method behavior not covered by test262.
---*/

// Uint8Array.prototype.toHex indexes the correct slice of the backing ArrayBuffer.
{
  const buffer = new ArrayBuffer(5);
  new Uint8Array(buffer).set([0xaa, 0xbb, 0xcc, 0xdd, 0xee]);

  // Open slice from the middle until the end of the buffer
  assert.sameValue(new Uint8Array(buffer, 1).toHex(), "bbccddee");

  // Fixed slice in the middle of the buffer
  assert.sameValue(new Uint8Array(buffer, 2, 3).toHex(), "ccddee");
}
