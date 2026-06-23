// OPTIONS: --run

(() => eval('this'))();

(() => eval('() => this'))();

(() => eval('eval("this")'))();