function testMemberCall(x) {
  x.foo();
  x.foo(1, 2, 3);

  // Receiver value is preserved across property and arg evaluation
  var z = 1 + 2;
  z.foo(4);
}