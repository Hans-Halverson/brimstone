/*---
description: Uint8Array base64 conversion method behavior not covered by test262.
---*/

// Uint8Array.prototype.toBase64 indexes the correct slice of the backing ArrayBuffer.
{
  const buffer = new ArrayBuffer(6);
  new Uint8Array(buffer).set([1, 2, 3, 4, 5, 6]);

  // Open slice from the middle until the end of the buffer
  assert.sameValue(new Uint8Array(buffer, 1).toBase64(), "AgMEBQY=");

  // Fixed slice in the middle of the buffer
  assert.sameValue(new Uint8Array(buffer, 2, 3).toBase64(), "AwQF");
}

// Verify behavior of all JS whitespace characters
{
  // Allowed whitespace characters
  assert.compareArray(Uint8Array.fromBase64("Zm9\tv"), [0x66, 0x6f, 0x6f]);
  assert.compareArray(Uint8Array.fromBase64("Zm9\nv"), [0x66, 0x6f, 0x6f]);
  assert.compareArray(Uint8Array.fromBase64("Zm9\fv"), [0x66, 0x6f, 0x6f]);
  assert.compareArray(Uint8Array.fromBase64("Zm9\rv"), [0x66, 0x6f, 0x6f]);
  assert.compareArray(Uint8Array.fromBase64("Zm9 v"), [0x66, 0x6f, 0x6f]);

  // Other whitespace characters are disallowed
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u000Bv"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u00A0v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u1680v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2000v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2001v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2002v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2003v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2004v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2005v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2006v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2007v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2008v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u2009v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u200Av"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u202Fv"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u205Fv"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\u3000v"));
  assert.throws(SyntaxError, () => Uint8Array.fromBase64("Zm9v\uFEFFv"));
}