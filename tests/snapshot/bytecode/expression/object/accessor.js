function getter() {
  return {
    get a() {},
    get "b"() {},
    get 1() {},
    get [2]() {},
  };
}

function setter() {
  return {
    set a(x) {},
    set "b"(x) {},
    set 1(x) {},
    set [2](x) {},
  };
}
