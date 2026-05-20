function foo() {
  eval('function bar() { throw new Error("test") }; bar()');
}

foo();