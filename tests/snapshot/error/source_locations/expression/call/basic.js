function foo() {
  bar();
}

function bar() {
  baz();
}

function baz() {
  throw new Error('This is an error');
}

foo();