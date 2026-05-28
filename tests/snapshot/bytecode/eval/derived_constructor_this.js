// OPTIONS: --run

class Base {}

class C1 extends Base {
    constructor() {
      eval('this');
    }
}

class C2 extends Base {
    constructor() {
      eval('this.fromEval = this.method();');
    }
}

class C3 extends Base {
  constructor() {
    eval('super()');
  }
}

class C4 extends Base {
  constructor() {
    eval('super.method()');
  }
}

try { new C1(); } catch {}
try { new C2(); } catch {}
try { new C3(); } catch {}
try { new C4(); } catch {}