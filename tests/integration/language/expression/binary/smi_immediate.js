/*---
description: Binary expressions where the right operand is a smi immediate.
---*/

function add(x) {
  return x + 3;
}

function addNegative(x) {
  return x + -3;
}

function sub(x) {
  return x - 3;
}

function mul(x) {
  return x * 3;
}

function mulLhs(x) {
  return 3 * x;
}

function div(x) {
  return x / 2;
}

function rem(x) {
  return x % 3;
}

function bitAnd(x) {
  return x & 6;
}

function bitAndLhs(x) {
  return 6 & x;
}

function bitOr(x) {
  return x | 6;
}

function bitOrLhs(x) {
  return 6 | x;
}

function bitXor(x) {
  return x ^ 6;
}

function bitXorLhs(x) {
  return 6 ^ x;
}

function shiftLeft(x) {
  return x << 2;
}

function shiftLeftOverflow(x) {
  return x << 34;
}

function shiftRightArithmetic(x) {
  return x >> 2;
}

function shiftRightArithmeticOverflow(x) {
  return x >> 34;
}

function shiftRightLogical(x) {
  return x >>> 2;
}

function shiftRightLogicalOverflow(x) {
  return x >>> 34;
}

// Smi operands
assert.sameValue(add(1), 4);
assert.sameValue(addNegative(1), -2);
assert.sameValue(sub(1), -2);
assert.sameValue(mul(-2), -6);
assert.sameValue(mulLhs(-2), -6);
assert.sameValue(div(7), 3.5);
assert.sameValue(rem(7), 1);
assert.sameValue(bitAnd(5), 4);
assert.sameValue(bitAndLhs(5), 4);
assert.sameValue(bitOr(5), 7);
assert.sameValue(bitOrLhs(5), 7);
assert.sameValue(bitXor(5), 3);
assert.sameValue(bitXorLhs(5), 3);
assert.sameValue(shiftLeft(5), 20);
assert.sameValue(shiftRightArithmetic(-8), -2);
assert.sameValue(shiftRightLogical(-8), 1073741822);

// Smi operands that overflow to doubles
assert.sameValue(add(2147483647), 2147483650);
assert.sameValue(sub(-2147483648), -2147483651);
assert.sameValue(shiftLeft(2147483647), -4);

// Overflowing shift operands are masked to 5 bits
assert.sameValue(shiftLeftOverflow(5), 20);
assert.sameValue(shiftRightArithmeticOverflow(-8), -2);
assert.sameValue(shiftRightLogicalOverflow(-8), 1073741822);

// Double operands
assert.sameValue(add(1.5), 4.5);
assert.sameValue(sub(1.5), -1.5);
assert.sameValue(mul(1.5), 4.5);
assert.sameValue(mulLhs(1.5), 4.5);
assert.sameValue(div(1.5), 0.75);
assert.sameValue(rem(7.5), 1.5);
assert.sameValue(bitAnd(5.7), 4);
assert.sameValue(bitAndLhs(5.7), 4);
assert.sameValue(bitOr(5.7), 7);
assert.sameValue(bitOrLhs(5.7), 7);
assert.sameValue(bitXor(5.7), 3);
assert.sameValue(bitXorLhs(5.7), 3);
assert.sameValue(shiftLeft(5.7), 20);
assert.sameValue(shiftRightArithmetic(-8.7), -2);
assert.sameValue(shiftRightLogical(-8.7), 1073741822);

// Multiplying by zero preserves the sign of the left operand
assert.sameValue(1 / mul(-0), -Infinity);
assert.sameValue(1 / mulLhs(-0), -Infinity);

// Operands that are not numbers
assert.sameValue(add('foo'), 'foo3');
assert.sameValue(sub('7'), 4);
assert.sameValue(mul('7'), 21);
assert.sameValue(mulLhs('7'), 21);
assert.sameValue(div('7'), 3.5);
assert.sameValue(rem('7'), 1);
assert.sameValue(bitAnd('5'), 4);
assert.sameValue(bitAndLhs('5'), 4);
assert.sameValue(bitOr('5'), 7);
assert.sameValue(bitOrLhs('5'), 7);
assert.sameValue(bitXor('5'), 3);
assert.sameValue(bitXorLhs('5'), 3);
assert.sameValue(shiftLeft('5'), 20);
assert.sameValue(shiftRightArithmetic('-8'), -2);
assert.sameValue(shiftRightLogical('-8'), 1073741822);
assert.sameValue(add({ valueOf: () => 39 }), 42);
