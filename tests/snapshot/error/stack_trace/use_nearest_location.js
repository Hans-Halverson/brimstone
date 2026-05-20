// Error comes from native Iterator.next, but the location in source message should point to
// the next method below.
var i = { next() { ii.next() } };
var ii = Iterator.prototype.map.call(i, x => x);
ii.next(); 