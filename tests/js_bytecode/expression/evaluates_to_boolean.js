// Check for expressions that are statically known to evaluate to a boolean. Can skip ToBoolean
// conversion in these cases.
function test(x, y) {
  if (false) {
    return 1;
  }

  if (x < y) {
    return 2;
  }
}