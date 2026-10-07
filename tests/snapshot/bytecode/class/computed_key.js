function testComputedKeyNotClobbered(param) {
  class C1 {
    [param] = 1;
  }
}
