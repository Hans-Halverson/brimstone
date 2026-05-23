var globalVar1 = 1;
const globalConst1 = 2;

{
  var globalVar2 = 3;
  const globalConst2 = 4;

  // Global loads
  globalVar2;
  unresolved;

  // Static load from scope
  globalConst2;

  eval('test');
}

{
  // Global loads
  globalVar2;
  globalConst2;
  unresolved;
}

function forcesDynamicInScopeAndParents(p1) {
  var v1 = 1;
  const c1 = 2;

  {
    var v2 = 3;
    const c2 = 4;

    // Static loads
    v1;
    v2;
    c1;
    c2;

    // Dynamic load as global could be shadowed
    globalVar1;

    // Dynamic load
    unresolved;

    eval('test');
  }

  {
    // Static load
    v1;
    v2;
    c1;

    // Dynamic load instead of load global
    unresolved + 5;

    // Dynamic load as global could be shadowed
    globalVar1;
  }

  // Dynamic load
  unresolved + 6;
}