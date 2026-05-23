({
  namedSuperMemberCall() {
    super.foo();
    super.foo(1, 2, 3);
  },

  computedSuperMemberCall() {
    super['foo']();
    super['foo'](1, 2, 3);
  },
});
