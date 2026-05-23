"use strict";

var obj = {};
Object.defineProperty(obj, 'foo', { value: 1, configurable: false });

delete obj.foo;