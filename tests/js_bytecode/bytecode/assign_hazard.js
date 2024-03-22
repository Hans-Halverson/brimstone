function simpleAssignHazard(param) {
  // Example of assignment hazard that is avoided. Param on lhs is immediately assigned to temporary
  // register so the rhs assignment does not overwrite it.
  return param + (param = 2);
}

function inOuterExpr(param) {
  if (param + (param = 2)) {}
}

function inRegularParam(p2, p1 = (p2 + (p2 = 2))) {}

function inRestParam(p1, ...{[p1 + (p1 = 2)]: rest }) {}

function inVarDeclarator(param) {
  var { x = param + (param = 2) } = {};
}

function inCatchParam(param) {
  try {
    1;
  } catch ({[param + (param = 2)]: p}) {
    3;
  }
}

function inForEachInit(param, x) {
  for ({[param + (param = 2)]: x} in {}) {}
}

function toplevelAssignExpressions(p1, p2) {
  // Not an assignment hazard
  p1 = p1 + 1;

  // Is an assignment hazard
  p2 = p1 + (p1 = 2);

  // Is a direct assignment, no hazard
  p2 = (p1 = 3);


}

function nestedFunctionOrClass(p1, p2) {
  // Not an assignment hazard
  p1 = p2 + ((x) => { x = 1 });
  p1 = p2 + function (x) { x = 2 };
  p1 = p2 + { f(x) { x = 3} };
}
