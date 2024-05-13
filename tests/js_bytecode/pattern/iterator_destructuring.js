function empty(p) {
  var [] = p;
}

function single(p) {
  var [a] = p;
}

function multiple(p) {
  var [a, b, c] = p;
}

function singleHole(p) {
  var [,] = p;
}

function multipleHoles(p) {
  var [,,,] = p;
}

function mixedHolesAndValues1(p) {
  var [a,,b] = p;
}

function mixedHolesAndValues2(p) {
  var [,a,,] = p;
}

function destructuring(p) {
  var [a = 1, {b}] = p;
}

function onlyRest(p) {
  var [...a] = p;
}

function valuesAndRest(p) {
  var [a, b, ...c] = p;
}

function restDestructuring(p) {
  var [...{b}] = p;
}

function elementEvaluationOrder() {
  ([a()[b()]] = c());
}

function restEvaluationOrder() {
  ([...a()[b()]] = c());
}

function *withYield(p) {
  var [a = yield] = p;
}