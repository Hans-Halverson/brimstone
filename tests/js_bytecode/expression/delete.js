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