function testEmptyArray() {
  return [];
}

function testArrayWithOneElement() {
  return [1];
}

function testArrayWithElements() {
  return [1, 2, 3];
}

function testArrayWithHolesAndElements() {
  return [1, , 2, , , 3, ,];
}

function testArrayWithOnlyHoles() {
  return [, , , ];
}

function onlySpread(p) {
  return [...p];
}

function multipleSpreads(p1, p2, p3) {
  return [...p1, ...p2, 1, ...p3];
}

function temporaryRegisterToAvoidClobbering() {
  var local = 1;
  // Intermediate array is written to a temporary to avoid clobbering
  local = [ 2 ];
}

function noTemporaryRegisterNeededForEmptyObject() {
  var local = 1;
  // Intermediate array is not needed if array is empty since no observable clobbering can occur
  local = [];
}