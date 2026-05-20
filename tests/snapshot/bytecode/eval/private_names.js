// OPTIONS: --run

class C1 {
  #field = 1;

  static method1() {
        eval('this.#field')
  }
}

C1.method1();