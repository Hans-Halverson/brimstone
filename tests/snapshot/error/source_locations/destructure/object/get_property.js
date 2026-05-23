var obj = {
  get foo() {
    throw new Error();
  }
};

var { ['fo' + 'o']: x } = obj;