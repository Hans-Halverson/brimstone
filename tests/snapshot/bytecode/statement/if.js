function testNoAlternate(x) {
  if (x) {
    return 1;
  }

  return 2;
}

function testAlternate(x) {
  if (x) {
    return 1;
  } else {
    return 2;
  }
}

function testNested(x, y) {
  if (x) {
    if (y) {
      return 1;
    } else {
      return 2;
    }
  } else {
    if (y) {
      return 3;
    } else {
      return 4;
    }
  }
}

function testIfElse(x, y, z) {
  if (x) {
    return 1;
  } else if (y) {
    return 2;
  } else if (z) {
    return 3;
  } else {
    return 4;
  }
}

function testIfElseNoCatchAll(x, y) {
  if (x) {
    return 1;
  } else if (y) {
    return 2;
  }

  return 3;
}

function testIfNoToBoolean(x) {
  if (x < x) {
    return 1;
  }

  if (x < x) {
    return 2;
  } else {
    return 3;
  }
}