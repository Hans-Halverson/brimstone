class B {}

class C extends B {
  deleteSuperProperty() {
    delete super.x;
  }
}

var x = new C();
x.deleteSuperProperty();