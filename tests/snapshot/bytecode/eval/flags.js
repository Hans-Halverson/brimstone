// No flags
eval('');

class C1 extends String {
  method1() {
    // Method flags
    eval('')
  }

  static method2() {
    // Static method flags
    eval('')
  }

  constructor() {
    // Derived constructor flags
    eval('')

    super();
  }

  // Field flags
  field1 = eval('');

  // Static initializer flags
  static {
    eval('');
  }

  // Static field flags
  static field2 = eval('');
}

class C2 {
  constructor() {
    // No derived constructor flag
    eval('')
  }
}