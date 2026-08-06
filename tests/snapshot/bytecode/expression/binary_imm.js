function simpleRhsImmediate(x) {
  x + 1;
  x - 2;
  x * 3;
  x / 4;
  x % 5;
  x & 6;
  x | 7;
  x ^ 8;
  x << 9;
  x >> 10;
  x >>> 11;
}

function simpleLhsImmediate(x) {
  // Supported operations for immediate LHS
  1 * x;
  2 & x;
  3 | x;
  4 ^ x;

  // Unsupported operations for immediate LHS
  5 + x;
  6 - x;
  7 / x;
  8 % x;
  9 << x;
  10 >> x;
  11 >>> x;
}

function negativeImmediate(x) {
  x + -1;
  x - -2;
}

function wideImmediate(x) {
  x + 1000;
  x + 100000;
}

function notAnImmediate(x) {
  1 + x;
  x ** 2;
  x + 1.5;
  x + 2147483648;
  x - -0;
}
