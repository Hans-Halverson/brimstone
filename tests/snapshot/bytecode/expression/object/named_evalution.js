function allNamedExpressions() {
  return {
    a: () => {},
    b: function() {},
    // Name not needed for already named function expression
    c: function alreadyNamed() {}
  }
}

function stringKey() {
  return { "test": () => {} };
}

function computedNeedsName() {
  return {
    [1 + 2]: () => {},
    3: () => {},
  }
}

function computedAlreadyNamed() {
  return { [1]: function alreadyNamed() {} };
}