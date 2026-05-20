function breakInTry(p1) {
  while (true) {
    try {
      if (p1) {
        break;
      }
    } finally {
      1;
    }
    2;
  }
}

function breakInCatch(p1) {
  while (true) {
    try {
      1;
    } catch {
      if (p1) {
        break;
      }
    } finally {
      2;
    }
    3;
  }
}

function breakInTryAndCatch(p1, p2) {
  while (true) {
    try {
      if (p1) {
        break;
      }
    } catch {
      if (p2) {
        break;
      }
    } finally {
      1;
    }
    4;
  }
}

function singleFinallyBreak(p1) {
  while (true) {
    try {
      1;
    } catch {
      2;
    } finally {
      // Break in a finally is not within the finally scope
      if (p1) {
        break
      }
    }
    5;
  }
}

function unwindsBreak(p1) {
  while (true) {
    const s1 = 1;
    {
      const s2 = 2;
      try {
        const s3 = 3;
        function inner() { return s1 + s2 + s3 }
        if (p1) {
          // No need to unwrap scopes in break path before jumping to finally
          break;
        }
      } finally {
        4;
        // But need to unwrap scopes in break path after finally
      }
    }
    5;
  }
}

function breakDoesNotLeaveFinally(p1) {
  try {
    while (true) {
      if (p1) {
        break;
      }
      1;
    }
  } finally {
    2;
  }
}


function breakDoesNotLeaveFinally(p1) {
  try {
    while (true) {
      if (p1) {
        break;
      }
      1;
    }
  } finally {
    2;
  }
}

function leavesNested(p1) {
  // Does not leave the outer try
  label: while (true) {
    try {
      while (true) {
        try {
          if (p1) {
            break label;
          }
        } finally {
          1;
        }
        2;
      }
    } finally {
      3;
    }
    4;
  }
}