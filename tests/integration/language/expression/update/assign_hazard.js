/*---
description: Assignment hazards in expression containing update expressions.
---*/

function prefixIncHazard(param) {
  return param + (++param);
}

function prefixDecHazard(param) {
  return param + (--param);
}

function postfixIncHazard(param) {
  return param + (param++);
}

function postfixDecHazard(param) {
  return param + (param--);
}

assert.sameValue(prefixIncHazard(10), 21);
assert.sameValue(prefixDecHazard(10), 19);
assert.sameValue(postfixIncHazard(10), 20);
assert.sameValue(postfixDecHazard(10), 20);
