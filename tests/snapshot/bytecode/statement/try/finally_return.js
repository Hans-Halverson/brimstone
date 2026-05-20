function singleTryReturn(p) {
  try {
    if (p) {
      return 1;
    }
  } finally {
    2;
  }
}

function singleUndefinedReturn(p) {
  try {
    if (p) {
      return;
    }
  } finally {
    1;
  }
}

function singleCatchReturn(p) {
  try {
    1;
  } catch (p) {
    if (p) {
      return 2;
    }
  } finally {
    3;
  }
}

function singleFinallyReturn(p) {
  try {
    1;
  } catch {
    2;
  } finally {
    // Return in a finally is not within the finally scope
    if (p) {
      return 3;
    }
  }
}

function returnsInTryAndCatch(p1, p2) {
  try {
    if (p1) {
      return 1;
    }
  } catch {
    if (p2) {
      return 2;
    }
  } finally {
    3;
  }
}

function multipleReturns(p1, p2, p3) {
  try {
    if (p1) {
      return 1;
    } else if (p2) {
      return 2;
    }
  } catch {
    if (p3) {
      return 3;
    }
  } finally {
    4;
  }
}

function multipleFinallyScopes(p1) {
  try {
    1;
    try {
      2;
      try {
        if (p1) {
          return 3;
        }
      } finally {
        4;
      }
    } finally {
      5;
    }
  } finally {
    6;
  }
}