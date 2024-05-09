class OnlyStaticInitializer {
  static {
    1 + 2;
    3 + 4;
  }
}

class StaticInitializerAfterCreateClass {
  method1() {}

  static {
    1 + 2;
  }
}

class MultipleStaticInitializers {
  static {
    1 + 2;
  }

  static {
    3 + 4;
  }

  static {
    5 + 6;
  }
}

class EvalInStaticInitializer {
  static {
    var x = 1;
    eval('');
  }
}

class SeparateVariableScopes {
  static {
    var x = 1;
  }

  static {
    var x = 2;
  }
}

class SpecialExpressionAccess {
  static {
    this + 1;
    new.target + 2;

    // Accesses the super home object
    super.foo + 3;
  }

  method() {
    // Create home object as well
    super.foo;
  }
}