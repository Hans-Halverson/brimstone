function shorthand(x) {
  var { a } = 1;
  var { b, c, d } = x;
}

function empty(x) {
  var {} = x;
}

function propertyAliases(x) {
  var { a: b } = x;
  var { b: c, d: e, f: g } = x;
}

function nested(x) {
  var { a: { b } } = x;
  var { c: { d, e: { f } } } = x;
}

function computed(x) {
  var { [1]: a } = x;
  var { [2]: { [3]: b } } = x;
}

function defaultValue(x, y) {
  var { a = 1 } = x;
  var { a: b = 1 } = x;

  var { c = y } = x;
  var { a: d = y } = x;

  var { [1]: a = 2 } = x;
}

function namedExpression() {
 var { a } = () => {};
 var { a = () => {}} = 1;
 var { a: b = () => {}} = 1;
}

function rest(x) {
  var { ...a } = x;
  var { a, b, ...e } = x;
  var { a, [1]: b, [2]: c, ...d } = x;
}

function propertyNamesNotResolved() {
  // No TDZ check needed
  const { a, b: { a: c } } = 1;
}

function tdzWithinDestructuring() {
  // TDZ check needed
  const { a, [a]: b } = 1;
}

function propertyEvaluationOrder() {
  ({ [a()]: b()[c()] } = d());
}

function restEvaluationOrder() {
  ({ a, ...(b()[c()]) } = d());
}
