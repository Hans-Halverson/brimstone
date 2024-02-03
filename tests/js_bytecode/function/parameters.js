function destructuring({a, b}, {c: d, e: f = 1}) {}

function defaultValues(a, b = 1, c = 2, d = b) {}

function rest(a, b = 1, ...c) {}

function restDestructuring(a, b, ...{c}) {}