let l1;
let l2 = 1;

const c1 = 2;

function noCaptures() {
  let l1;
  let l2 = 1;
  const c1 = 2;
}

function captures() {
  let l1;
  let l2 = 1;
  const c1 = 2;

  function inner() {
    return l1 + l2 + c1;
  }
}