// Global destructuring
var { v1, z: v2, [2]: v3, ...v4 } = 1;

function constants() {
  const { a, z: b, [2]: c, ...d } = 1;
}

function nestedMember(x) {
   ({ a: x.b } = 1);
}

({
  superMember(x) {
   ({ a: super.x } = 1);
  }
});
