var x = 1;

// Initialize const and let to empty at scope start
const c1 = 2;
let l1 = 3;

function test1() {
  // Load from global var
  x;

  // Load from global function
  test1;
  
  // Load from unresolved global
  {
    unresolved;
  }
}

{
  // Block scope can be greated in global scope
  const c1 = 1;
  function inner() {
    return c1;
  }
}