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

function reassignIteratorSource(p) {
  var [a, b] = a;
}

function firstArrayPatternDoesNotThrow(p) {
  var [[a]] = p;
}

function firstObjectPatternDoesNotThrow(p) {
  var [{a}] = p;
}

function firstMemberPatternMayThrow(p) {
  var a;
  ([a.b] = p);
}

function firstAssignPatternDoesNotThrow(p) {
  var [a = 1] = p;
}

function firstAssignPatternMayThrow(p) {
  var a;
  ([a.b = 1] = p);
}

function firstRestPatternDoesNotThrow(p) {
  var [...a] = p;
}

function firstRestPatternMayThrow(p) {
  var a;
  ([...a.b] = p);
}