function evaluationOrder() {
  return (1 + 2)`test${3 + 4}test${5 + 6}`;
}

function noArgs(tag) {
  return tag`test`;
}

function argsInContiguousRegisters(tag) {
  return tag`a${1}b${2}c${3}`;
}

function allExpressionTypes(tag, x) {
  // Arguments are literals, registers, and globals
  return tag`${1} ${x} ${allExpressionTypes}`;
}

function namedMemberTag(x) {
  x.foo`${1}${2}`;

  // Receiver value is preserved across property and arg evaluation
  var z = 1 + 2;
  z.foo`${3}`;
}

function computedMemberTag(x) {
  x['foo']`${1}${2}`;

  // Receiver value is preserved across property and arg evaluation
  var z = 1 + 2;
  z['foo']`${3}`;
}

function optionalNamedMemberTag(x) {
  (x?.foo)`${1}`;
  (x?.a.b)`${1}`;
}

function optionalComputedMemberTag(x) {
  (x?.[1])`${1}`;
  (x?.[1][2])`${1}`;
}