class B {
  constructor() {
    throw new Error()
  }
}

class C extends B {}

// Default derived constructor error points to start of the class
new C();