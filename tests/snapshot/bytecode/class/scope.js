function notCaptured() {
  class C {
    method() {}
  }
}

function capturedByMethod() {
  class C {
    method() {
      C;
    }
  }
}

function capturedByStaticInitializer() {
  class C {
    static {
      C;
    }
  }
}

// Class expression captured
function classExpressionCaptured() {
  (class C {
    method() {
      C;
    }
  });
}

// Eval captures class name in body
function capturedByEval() {
  class C {
    method() {
      eval('');
    }
  }
}

function canReassignOutsideBodyButNotInside() {
  class C {
    method() {
      C = 1;
    }
  }

  C = 2;
}