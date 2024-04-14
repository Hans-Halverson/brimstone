function test1(x, y) {
  x + y;
  x - y;
  x * y;
  x / y;
  x % y;
  x ** y;
  x == y;
  x != y;
  x === y;
  x !== y;
  x < y;
  x <= y;
  x > y;
  x >= y;
  x & y;
  x | y;
  x ^ y;
  x << y;
  x >> y;
  x >>> y;
  'x' in y;
  x instanceof y;
}

function privateIn() {
  class C {
    #field = 1;

    method() {
      #field in this;
    }
  }
}