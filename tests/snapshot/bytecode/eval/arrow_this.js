// OPTIONS: --run

(() => eval('this'))();

(() => eval('() => this'))();

(() => eval('eval("this")'))();

class Base {}
class Derived extends Base {
  constructor() {
    super();
    (() => eval('this'))();
    (() => eval('() => this'))();
  }
}

new Derived();