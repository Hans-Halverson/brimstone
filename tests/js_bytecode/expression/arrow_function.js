function basic() {
  (() => 1) + 2;

  (() => {
    var y = 9;
    return y + 3;
  }) + 4;

  var x = () => 5;
}

function named() {
  // Named
  var x = () => {}
  var y = 1;
  y = () => {};
}