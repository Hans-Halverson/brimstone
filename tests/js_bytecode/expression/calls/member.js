function testNamedMemberCall(x) {
  x.foo();
  x.foo(1, 2, 3);

  // Receiver value is preserved across property and arg evaluation
  var z = 1 + 2;
  z.foo(4);
}

function testComputedMemberCall(x) {
  x['foo']();
  (0 + 9)['foo'](1, 2, 3);

  // Receiver value is preserved across property and arg evaluation
  var z = 1 + 2;
  z['foo'](4);
}

function testOptionalNamedMemberCall(x) {
  (x?.foo)();
  (x?.a.b)();
}

function testOptionalComputedMemberCall(x) {
  (x?.[1])();
  (x?.[1][2])();
}