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

function capturedParameters(x, {y}, z = 2, ...rest) {
  function inner() {
    return x + y + z + rest;
  }

  x = 1;
  y = 2;
  z = 3;
  rest = 4;
}

function capturedParameterNeedsTdzCheck(a = (() => a)(), b = (() => { b = 1 })()) {}

function capturedParameterBetweenInitializers(a = () => b, b = () => a) {}

function capturedParameterFromEvalInInitializers(a, b, c = eval('')) {}