function test(bar) {
  var foo = "foo";

  // Use bindings directly in computed property names. These must be moved to a temporary so that
  // they appear in the contiguous range of registers passed to NewClass.
  class C {
    [foo]() {}
    get [bar]() {}
  }
}