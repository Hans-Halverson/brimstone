// Check for expressions that are statically known to evaluate to a boolean. Can skip ToBoolean
// conversion in these cases.
function test(x, y) {
  if (false) {
    return 1;
  }

  if (x == y) {
    return 2;
  } else if (x != y) {
    return 3;
  } else if (x === y) {
    return 4;
  } else if (x !== y) {
    return 5;
  }

  if (x < y) {
    return 6;
  } else if (x <= 2) {
    return 7;
  } else if (x > 3) {
    return 8;
  } else if (x >= 4) {
    return 9;
  }

  if (!1) {
    return 10;
  }
}