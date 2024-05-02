function methods() {
  class C {
    #method1() {}
    static #method2() {}
  }
}

function individualAccessors() {
  class C {
    get #getter1() {}
    set #setter1(v) {}
    static get #getter2() {}
    static set #setter2(v) {}
  }
}

function accessorPairs() {
  class C {
    get #acc1() {}
    set #acc1(v) {}

    static set #acc2(v) {}
    static get #acc2() {}

    method1() {}
    get #acc3() {}
    method2() {}
    set #acc3(v) {}
    method3() {}
  }
}