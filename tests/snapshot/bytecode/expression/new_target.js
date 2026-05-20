function simpleNewTarget() {
  return new.target + 1;
}

function captured() {
  () => { new.target };
  return new.target + 1;
}

function usesInner() {
  function inner() { var x = 1; return new.target + 2 }
  return new.target + 3;
}