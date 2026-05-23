function foo() { a; }

(function foo() { a; });

(function () {});

(function (param) {});

async function foo() {}

function *foo() {}

async function *foo() {}

(async function *foo() {});

(async function *() {});

function foo(a, b, c) {}

function foo(a, b, c,) {}

function foo(a, b, ...c) {}

function foo(a, ...[b, ...c]) {}