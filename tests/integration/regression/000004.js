/*---
description: GC safety bug with Map and Set methods when key is a concat string that gets flattened
---*/

const map = new Map();
map.delete('a' + 'b');
map.has('a' + 'b');
map.get('a' + 'b');
map.set('a' + 'b', 1);

const set = new Set();
set.add('a' + 'b');
set.delete('a' + 'b');
set.has('a' + 'b');

const weakMap = new WeakMap();
weakMap.delete('a' + 'b');
weakMap.has('a' + 'b');
weakMap.get('a' + 'b');

const weakSet = new WeakSet();
weakSet.delete('a' + 'b');
weakSet.has('a' + 'b');