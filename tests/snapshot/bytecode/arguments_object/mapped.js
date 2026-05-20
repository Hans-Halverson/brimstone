function noArguments() {
  arguments;
}

function withArguments(x, y, z) {
  arguments;
}

function withDuplicateArguments(x, y, x) {
  // Shadowed argument still loaded to scope
  arguments;
}

function otherScopeBindings(p1, p2) {
  arguments;
  var x = 1;
  var y = 2;
  (() => x + y + this);
}