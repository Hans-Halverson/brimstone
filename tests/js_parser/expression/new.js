new a;

new a();

new a(b,);

new a(b, c, d);

new a(b, c, d,);

new a(...b);

new a + b;

new a.b;

new a.b();

new a(b)(c)(d);

new new a;

new new a();

new a().b;

// Meta properties and super properties are allowed as MemberExpressions
new import.meta;
new new.target;
new super.foo;