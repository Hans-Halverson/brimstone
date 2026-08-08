/*---
description: >
  TypedArray.prototype.copyWithin handles its ArrayBuffer being shrunk while coercing arguments.
---*/

// The target and start indices are resolved against the original length, but the
// buffer is shrunk during coercion of the end argument. The copy must clamp to the
// smaller buffer rather than underflowing the remaining-length computation.
const rab = new ArrayBuffer(100, { maxByteLength: 100 });
const ta = new Uint8Array(rab);
for (let i = 0; i < 100; i++) {
  ta[i] = i;
}

const shrinkToEmpty = {
  valueOf() {
    rab.resize(8);
    return 100;
  },
};

// Both indices end up past the new length, so nothing is copied and the surviving
// elements are untouched.
ta.copyWithin(50, 0, shrinkToEmpty);
assert.sameValue(ta.length, 8);
for (let i = 0; i < 8; i++) {
  assert.sameValue(ta[i], i);
}

// A shrink that still leaves both indices in bounds performs a copy clamped to the
// smaller buffer.
const rab2 = new ArrayBuffer(100, { maxByteLength: 100 });
const ta2 = new Uint8Array(rab2);
for (let i = 0; i < 100; i++) {
  ta2[i] = i;
}

const shrinkToTwenty = {
  valueOf() {
    rab2.resize(20);
    return 100;
  },
};

ta2.copyWithin(5, 0, shrinkToTwenty);
assert.sameValue(ta2.length, 20);

// ta2[0..5] is unchanged
for (let i = 0; i < 5; i++) {
  assert.sameValue(ta2[i], i);
}

// ta2[5..20] receives ta2[0..15]
for (let i = 5; i < 20; i++) {
  assert.sameValue(ta2[i], i - 5);
}
