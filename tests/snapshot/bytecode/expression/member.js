function testNamedMember(x) {
  x.foo;
  x.x;
}

function testComputedMember(x) {
  x['foo'];
  (1 + 2)[3 + 4];
}

function propertyNamesNotResolved(x) {
  // No TDZ check needed
  const a = x.a;
}