var x = 1;

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