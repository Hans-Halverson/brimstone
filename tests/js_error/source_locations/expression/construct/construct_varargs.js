function foo() {
  throw new Error();
}

(new foo(1, ...[]));