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