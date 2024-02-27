function basicTryFinally() {
  1;
  try {
    2;
  } finally {
    3;
  }
  4;
}

function basicTryCatchNoParamFinally() {
  1;
  try {
    2;
  } catch {
    3;
  } finally {
    4;
  }
  5;
}

function basicTryCatchWithSimpleParamFinally() {
  1;
  try {
    2;
  } catch (e) {
    3;
  } finally {
    4;
  }
  5;
}

function basicTryCatchWithComplexParamFinally() {
  1;
  try {
    2;
  } catch ({ e }) {
    3;
  } finally {
    4;
  }
  5;
}

function nested() {
  1;
  try {
    2;
    try {
      3;
    } finally {
      4;
    }
  } finally {
    5;
  }
  6;
}