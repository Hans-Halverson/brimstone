function startScope() {
  x + y;
  const x = 1;
  const y = 2;
}

function captures() {
  const x = 1;
  function inner() {
    return x;
  }
}