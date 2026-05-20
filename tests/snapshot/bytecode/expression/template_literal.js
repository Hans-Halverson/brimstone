function emptySingleQuasi(param) {
  param = ``;
  return ``;
}

function emptySingleExpression(param) {
  param = `${1}`;
  return `${1}`;
}

function emptyFollowedByExpression(param) {
  param = `${1}test`;
  return `${1}test`;
}

function expressionFollowedByEmpty() {
  return `test${1}`;
}

function expressionsAndEmptyLiterals() {
  return `${1}${2}${3}`;
}

function expressionsAndNonEmptyLiterals() {
  return `a${1}b${2}c`;
}

function fixedRegisterExpressions(p1, p2) {
  p2 = `${p1}`;
  p2 = `${p2}`;
  return `a${p1}b${p2}c`;
}