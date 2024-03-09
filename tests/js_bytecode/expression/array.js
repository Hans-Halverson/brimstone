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