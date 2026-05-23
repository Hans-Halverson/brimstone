// OPTIONS: --run

class C1 {
  method1() {
    // Nested eval inherits method flag
    eval('eval("")')
  }

  method2() {
    // Nested eval does not inherit method flag
    eval('(function () { eval("") })()')
  }
}

var c1 = new C1();
c1.method1();
c1.method2();