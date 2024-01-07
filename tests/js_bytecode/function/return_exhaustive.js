function testIfNoAlternate() {
  if (true) {
    return 1;
  }
  
  // Needs implicit return
}

function TestIfNormalAlternate() {
  if (true) {
    return 1;
  } else {
    2;
  }

  // Needs implicit return
}


function testIfNormalConseq() {
  if (true) {
    1;
  } else {
    return 2;
  }

  // Needs implicit return
}

function testIfBothAbrupt() {
  if (true) {
    return 1;
  } else {
    return 2;
  }

  // No implicit return
}

function testIfNestedOneNormalBranch() {
  if (true) {
    if (true) {
      return 1;
    } else {
      if (true) {
        return 2;
      } else {
        3;
      }
    }
  } else {
    return 4;
  }

  // Needs implicit return
}

function testIfNestedNoNormalBranches() {
  if (true) {
    if (true) {
      return 1;
    } else {
      if (true) {
        return 2;
      } else {
        return 3;
      }
    }
  } else {
    return 4;
  }

  // No implicit return
}