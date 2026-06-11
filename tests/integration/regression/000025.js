/*---
description: RegExp surrogate pairs with different escape formats.
---*/

// Forms a surrogate pair
assert(/\ud83d\ude0a/u.test('😊'));

// Not counted as a surrogate pair
assert.sameValue(/\ud83d\u{de0a}/u.test('😊'), false);
assert.sameValue(/\u{d83d}\ude0a/u.test('😊'), false);
assert.sameValue(/\u{d83d}\u{de0a}/u.test('😊'), false);
