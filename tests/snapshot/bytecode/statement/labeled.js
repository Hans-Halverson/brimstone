function completion() {
  label: {
    throw 1;
  }

  // Always emit next statement even if labeled statement is abrupt
  2;
}