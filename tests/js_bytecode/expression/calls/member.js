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