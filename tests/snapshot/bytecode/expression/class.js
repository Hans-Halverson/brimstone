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

function storeToParam(p) {
  p = class C {};
}

function namedEvaluation() {
  var x = class {};
  var x = class C {};
}