function startScope() {
  for (const x in {}) {
    x + inner;
    const inner = 0;
  }
}
