function deleteMember(param) {
    delete param.foo;
    delete param.inner.foo;
    delete param[1]
}

function deleteChainMember(param) {
  delete param?.foo;
  delete param?.[1];
  delete param?.foo.bar?.baz;
}

function deleteOtherExpression() {
  delete 1;
  delete (1 + 2);
}

var globalVar = 1;

function deleteIdVars(p) {
  var localVar = 1;

  delete localVar;
  delete globalVar;
}

function deleteIdFunctions() {
  function inner() {}

  delete inner;
  delete deleteIdFunctions;
}

function deleteIdLexical() {
  const x = 1;
  let y = 2;

  delete x;
  delete y;
}

function deleteIdParams(a) {
  delete a;

  try {
    1;
  } catch (b) {
    delete b;
  }
}

function deleteIdArgments() {
  delete arguments;
}

function deleteThis() {
  delete this;
}

function deleteUnresolvedGlobal() {
  delete unresolved;
}

function deleteUnresolvedDynamic() {
  eval(``);
  delete unresolved;
}

({
  deleteNamedSuperProperty() {
    return delete super.x;
  },

  deleteComputedSuperProperty() {
    return delete super[1 + 2];
  },
});