// In global scope
eval('test');

function simple() {
  return eval('test');
}

function parenthesizedEval() {
  // Still a direct eval
  return (eval)('test');
}

function indirect() {
  return eval?.('test');
}

function noArgs() {
  return eval();
}

function extraArgs() {
  return eval(1, 2, 3);
}