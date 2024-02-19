function startScope(p) {
  // Separate scopes for with and block
  with (p) {
    const x = 1;
    const y = 2;

    function inner() {
      return x + y;
    }
  }
}

function dynamicAccessInChildren(p) {
  var outside = 1;

  with (p) {
    // Dynamic load and store
    outside + 2;
    outside = 0;

    // Static load
    let resolved = 3;
    resolved + 4;

    // Dynamic load
    unresolved + 5;

    {
      // Static load
      resolved + 6;

      // Dynamic load
      unresolved + 7;
    }
  }
}

function forcesParentsToBeCaptured(p1, p2) {
  var outside = 1;
  arguments;
  with (p1) {}
}