/*---
description: >
  parseInt returns the correctly rounded value of the exact integer for radix 10 and power-of-two
  radixes, and a close approximation for other radixes when the integer does not fit in 64 bits.
---*/

function exactValue(string, radix) {
  let value = 0n;
  for (const char of string) {
    value = value * BigInt(radix) + BigInt(parseInt(char, 36));
  }
  return Number(value);
}

const exactCases = [
  // Fits in a u64
  ["4840248844602088288", 10, 4840248844602088000],
  ["18446744073709551615", 10, 2 ** 64],
  ["3000000000000908", 16, 3458764513820543500],
  ["18446462598732840000", 10, 18446462598732840960],
  ["zz", 36, 1295],

  // Overflows a u64 in radix 10
  // u64::MAX + 1: the multiply fits but the final add overflows
  ["18446744073709551616", 10, 2 ** 64],
  ["18446744073709551617", 10, 2 ** 64],
  ["382608048280408202426", 10, 382608048280408200000],
  ["99999999999999999999", 10, 1e20],
  ["123456789012345678901234567890", 10, 1.2345678901234568e29],
  ["1".repeat(400), 10, Infinity],

  // Overflows a u64 in a power-of-two radix
  ["10000000000000000", 16, 2 ** 64],
  ["10000000000000000000000000000000000000000000000000000100000000001", 2, 2 ** 64 + 2 ** 12],
  ["200000000000000000000000001000001", 4, 2 ** 65 + 2 ** 13],
  ["4000000000000000010001", 8, 2 ** 65 + 2 ** 13],
  ["80000000000004001", 16, 2 ** 67 + 2 ** 15],
  ["30800000000009080", 16, 55916692973432120000],
  ["G0000000002001", 32, 2 ** 69 + 2 ** 17],
  ["f".repeat(300), 16, Infinity],
];

for (const suffix of ["", "!9", "Ā"]) {
  for (const [string, radix, expected] of exactCases) {
    assert.sameValue(parseInt(string + suffix, radix), expected);
    assert.sameValue(parseInt("  -" + string + suffix, radix), -expected);
  }

  assert.sameValue(parseInt("0x80000000000004001" + suffix), 2 ** 67 + 2 ** 15);
  assert.sameValue(parseInt("0X80000000000004001" + suffix, 16), 2 ** 67 + 2 ** 15);
  assert.sameValue(parseInt("-0" + suffix), -0);

  // Other radixes may be approximated when overflowing a u64
  for (const [string, radix] of [
    ["zzzzzzzzzzzzzzzzzz", 36],
    ["ZzZzZzZzZzZzZzZzZz", 36],
    ["12121212121212121212121212121212121212121212", 3],
    ["66666666666666666666666666", 7],
  ]) {
    const expected = exactValue(string, radix);
    const actual = parseInt(string + suffix, radix);
    assert(Math.abs(actual - expected) <= expected * 2 ** -50);
  }

  assert.sameValue(parseInt("z".repeat(300) + suffix, 36), Infinity);
}
