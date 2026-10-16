// Parameter expressions use parameter scope, body expressions use body scope
function sameNameNoCaptures(x = 1, y = x) {
  var x = 2;
  x + 3;
}

function sameNameCaptureParamOnly(x = 1, y = () => x) {
  var x = 2;
  x + 3;
}

function sameNameCaptureBodyOnly(x = 1, y = x) {
  var x = 2;
  function inner() {
    return x;
  }
}

function sameNameBothCaptures(x = 1, y = () => x) {
  var x = 2;
  function inner() {
    return x;
  }
}

function tdzAfterParams(x = 1) {
  const y = 2;
  function inner() {
    return y;
  }
}

function bodyScopeIfParamsHaveEval(x = eval('')) {
  1;
}

function argumentsNoCapture(x = 1, y = arguments) {
  var arguments = 2;
  arguments + 3;
}

function argumentsCapture(x = 1, y = arguments) {
  var arguments = 2;
  (() =>  arguments);
}