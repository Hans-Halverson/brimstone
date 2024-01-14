function testAbruptBodyOnly() {
  try {
    return;
  } catch {
    1;
  }

  // Needs implicit return
}

function testAbruptCatchOnly() {
  try {
    1;
  } catch {
    return;
  }

  // Needs implicit return
}

function testAbruptFinallyOnly() {
  try {
    1;
  } finally {
    return;
  }

  // No implicit return
}

function testAbruptCatchAndBody() {
  try {
    return;
  } catch {
    return;
  }

  // No implicit retun
}