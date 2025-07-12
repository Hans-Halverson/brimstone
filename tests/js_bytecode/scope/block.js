function startScope() {
  const x = 1;
  let y = 2;

  {
    // TDZ check needed, but separate VM locations
    const x = x;
    let y = y;
  }
}

function captures() {
  {
    const x = 1;
    function foo() {
      return x;
    }
  }
}

let outerScope = 1;

function abruptEndReturn() {
  block: {
    let x = 1;
    (() => x);

    // Force a path that leaves the block
    if (true) {
      break block;
    }

    // Abrupt (return) completion
    return;
  }

  // Correct outer scope
  outerScope;
}


function abruptEndThrow() {
  block: {
    let x = 1;
    (() => x);

    // Force a path that leaves the block
    if (true) {
      break block;
    }

    // Abrupt (throw) completion
    throw 2;
  }

  // Correct outer scope
  outerScope;
}

function abruptEndBreak() {
  block: {
    let x = 1;
    (() => x);

    // Abrupt (break) completion
    break block;
  }

  // Correct outer scope
  outerScope;
}