// Toplevel var functions placed in globals
function test1() {}

{
  // Toplevel decl functions placed in local
  function test2() {}
}

function test3() {
  // Nested functions placed in locals
  function test4() {}
  return
}