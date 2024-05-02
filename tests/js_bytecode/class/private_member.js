class C {
  #method() {}

  privateMember() {
    return this.#method;
  }

  privateCall() {
    return this.#method();
  }

  privateOptionalMember() {
    return this.#method?.();
  }

  privateOptionalCall() {
    return this.#method?.();
  }

  privateAssignment(x) {
    this.#method = x;
  }

  privateCompoundAssignment(x) {
    this.#method += x;
  }

  privateUpdate() {
    this.#method++;
  }

  privateDestructure(x) {
    ({ x: this.#method } = x);
  }
}
