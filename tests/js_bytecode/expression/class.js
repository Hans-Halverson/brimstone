function emptyAnonymous() {
  return class {};
}

function emptyNamed() {
  return class C {};
}

function nonEmptyAnonymous() {
  return class {
    method() {
      return 1;  
    }

    static {
      2;
    }
  };
}

function nonEmptyNamed() {
  return class C {
    method() {
      return 1;  
    }

    static {
      2;
    }
  };
}