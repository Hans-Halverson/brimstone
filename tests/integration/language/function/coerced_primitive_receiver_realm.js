/*---
description: Coerced primitive receiver is created in the calle's realm.
---*/

var otherRealm = $262.createRealm().global;
var returnThis = new otherRealm.Function("return this;");

Boolean.prototype.bool = returnThis;
Number.prototype.number = returnThis;
String.prototype.string = returnThis;
Symbol.prototype.symbol = returnThis;
BigInt.prototype.bigint = returnThis;

assert.sameValue(true.bool().constructor, otherRealm.Boolean);
assert.sameValue((1).number().constructor, otherRealm.Number);
assert.sameValue("abc".string().constructor, otherRealm.String);
assert.sameValue(Symbol().symbol().constructor, otherRealm.Symbol);
assert.sameValue((1n).bigint().constructor, otherRealm.BigInt);
