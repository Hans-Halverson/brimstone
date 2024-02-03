var global = 0;

function emptyObject() {
  return {};
}

function shorthandProperties(param) {
  var local = 1;
  return { global, local, param };
}

function initializedProperties() {
  return { a: 1, global: 2 + 3 };
}

function stringPropertyKeys() {
  return { "a": 1, "b": 2 };
}

function computedPropertyKeys() {
  return { ["a"]: 1, 2: 3, [4 + 5]: 6 };
}

function spread(a) {
  return { ...a, ...(1 + 2) };
}

function propertyNamesNotResolved() {
  // No TDZ check needed
  const { a } = { a: 1 };

  // TDZ check needed
  const { b, c } = { b, d: c };
}