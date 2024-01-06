// Toplevel var functions placed in globals
function test1() { return }

{
  // Toplevel decl functions placed in local
  function test2() { return }
}

function test3() {
  // Nested functions placed in locals
  function test4() { return }
  return
}