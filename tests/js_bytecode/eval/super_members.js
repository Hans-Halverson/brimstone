// OPTIONS: --run

// Check that super member expression in eval references the correct home object.
class C extends String {
  field = eval('super.member');
  static field = eval('super.member');

  method1() {
    eval('super.method');
  }

  static method2() {
    eval('super.method');
  }
}

var x = new C();
x.method1();
C.method2();