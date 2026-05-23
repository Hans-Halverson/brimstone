({
  named() {
    return super.x;
  },

  computed() {
    return super[1 + 2];
  },

  captured() {
    () => super.x;
  },
});