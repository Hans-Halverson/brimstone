class Base {}

class C1 extends Base{
  constructor() {
    let x;

    // this must be loaded to temporary, perform init check, then be written to x
    x = this;
  }
}