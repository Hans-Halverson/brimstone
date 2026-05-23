function continueInTry(p1) {
  while (true) {
    try {
      if (p1) {
        continue;
      }
    } finally {
      1;
    }
    2;
  }
}

function continueInCatch(p1) {
  while (true) {
    try {
      1;
    } catch {
      if (p1) {
        continue;
      }
    } finally {
      2;
    }
    3;
  }
}

function continueInTryAndCatch(p1, p2) {
  while (true) {
    try {
      if (p1) {
        continue;
      }
    } catch {
      if (p2) {
        continue;
      }
    } finally {
      1;
    }
    4;
  }
}

function singleFinallyContinue(p1) {
  while (true) {
    try {
      1;
    } catch {
      2;
    } finally {
      // Continue in a finally is not within the finally scope
      if (p1) {
        continue
      }
    }
    5;
  }
}

function unwindsContinue(p1) {
  while (true) {
    const s1 = 1;
    {
      const s2 = 2;
      try {
        const s3 = 3;
        function inner() { return s1 + s2 + s3 }
        if (p1) {
          // No need to unwrap scopes in continue path before jumping to finally
          continue;
        }
      } finally {
        4;
        // But need to unwrap scopes in continue path after finally
      }
    }
    5;
  }
}

function continueDoesNotLeaveFinally(p1) {
  try {
    while (true) {
      if (p1) {
        continue;
      }
      1;
    }
  } finally {
    2;
  }
}


function continueDoesNotLeaveFinally(p1) {
  try {
    while (true) {
      if (p1) {
        continue;
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
            continue label;
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