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