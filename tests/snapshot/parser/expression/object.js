// Block statement
{};

({});

({ a: b });

({ 1: b, 1n: b });

({ "prop": b });

({ [key]: b });

({ a, b, c });

({ a: b, c: d, e: f });

({ a: b, c: d, e: f, });

// Methods

({ a() {} });

({ a(b,) {}, });

({ async a() {}, *a() {}, async *a() {} });

({ [a]() {}, 1() {}, "test"() {} });

// Keywords as shorthand properties
({ get, set, async });

({ get: 1, set: 1, async: 1 });

// Keywords as methods
({ get() {}, set() {}, async() {} });

({ async get() {}, async set() {}, async async() {} });

({ get a() {}, set a(b) {} });

({ get [a]() {}, get 1() {}, get "a"() {} });

({ get get() {}, get set() {}, set get(x) {}, set set(x) {} });

// Spread elements
({a, ...b, ...c});

({...a, b, ...c,});
