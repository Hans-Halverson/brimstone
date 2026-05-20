({ a } = 1);

({ a, b, } = 1);

({ "prop": a, 99: b, [c + 1]: d } = 1);

({ "prop": a = 1, 99: b = 2, [c + 1]: d = 3 } = 1);

({ a: [b, c], d: { e, f } = 2 } = 1);

({ a = 1 } = 1);

({ a: b = 1 } = 1);

({ get, set, async } = 1);

({ get: a, set: b, async: c } = 1);

({ get: a = 1, set: b = 2, async: c = 2 } = 1);

({ a, ...b} = 1);

({ ...a.b} = 1);