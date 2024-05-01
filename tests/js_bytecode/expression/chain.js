function namedMember(a) {
  -(a?.b);
  -(a?.b?.c);
  -(a?.b.c);
  -(a.b?.c);
}

function computedMember(a) {
  -(a?.[1]);
  -(a?.[1]?.[2]);
  -(a?.[1][2]);
  -(a[1]?.[2]);
}

function call(a) {
  -(a?.());
  -(a?.b?.());
  -(a?.b());
  -(a.b?.());
}

function nested(a) {
  -(a?.b?.c?.d?.e);
  -(a.b?.c.d?.e);
  -(a.b?.c().d?.e());
  -(a?.b?.c?.()?.d?.e?.());
}

({
  superMember() {
    -(super.a?.b);
    -(super.a?.());
  }
});