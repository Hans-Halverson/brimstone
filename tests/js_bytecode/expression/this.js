function use() {}

function foo() {
  // This to a local register
  var x = this;

  // Moving this to a any temporary
  this + 1;

  // Moving this to a new temporary
  use(this);
}