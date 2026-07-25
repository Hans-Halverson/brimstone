function distinctNames() {
  ({ a: 1, b: 2, c() {}, "d": 4 });
}

function duplicateNames() {
  ({ a: 1, b: 2, a: 3, "a": 4 });
}

function gettersAndSettersOvercounted() {
  ({ get a() {}, set a(x) {}, b: 2 });
}

function computedPropertiesCounted() {
  ({ [1]: 1, [2]: 2 });
}

function spreadIgnored() {
  ({ ...a, b: 2 });
}

function prototypeSetterIgnored() {
  ({ __proto__: null, b: 2 });
  ({ "__proto__": null, b: 2 });
}