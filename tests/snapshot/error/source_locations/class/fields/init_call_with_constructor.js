// Constructor, so source location for fields init call is the start of the constructor
class C {
  field = 1 + 2n;

  constructor() {}
}

new C();