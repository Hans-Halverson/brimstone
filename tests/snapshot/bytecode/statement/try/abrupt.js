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

function testAbruptCatchAndBody() {
  try {
    return;
  } catch {
    return;
  }

  // No implicit retun
}

function testAbruptFinallyInFinallyOnly() {
  try {
    1;
  } finally {
    throw 2;
  }

  // No implicit return or finally footer
  3;
}


function testAbruptTryInFinallyOnly() {
  try {
    throw 1;
  } finally {
    2;
  }

  // Needs implicit return
}

function testAbruptCatchInCatchFinally() {
  try {
    1;
  } catch {
    throw 2;
  } finally {
    3;
  }

  // Needs implicit return
}

function testAbruptCatchAndFinally() {
  try {
    throw 1;
  } catch {
    throw 2;
  } finally {
    3;
  }

  // No implicit return
}