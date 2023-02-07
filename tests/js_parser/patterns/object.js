var {} = 1;

var { a } = 1;

var { a, } = 1;

var { a, b, c } = 1;

var { a, b, c, } = 1;

var { a: b, c: d } = 1;

var { [a]: b, 1: d, "test": e, 1n: f } = 1;

var { a = 1 } = 2;

var { a: b = 1 } = 2;

var { a: { b: c, d: [e] } } = 1;

var { a, b, ...c } = 1;