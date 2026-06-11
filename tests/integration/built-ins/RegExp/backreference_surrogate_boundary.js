/*---
description: In unicode mode a backreference cannot match by splitting a surrogate pair.
---*/

// In unicode mode the backreference fails to match, both forwards and backwards
assert.sameValue(/(\ud83d)\1/u.exec("\ud83d😊"), null);
assert.sameValue(/(\ude0a)😊(?<=\1)/u.exec("\ude0a😊"), null);

// In non-unicode mode the backreference matches since surrogates are not paired
assert.compareArray(/(\ud83d)\1/.exec("\ud83d😊"), ["\ud83d\ud83d", "\ud83d"]);
assert.compareArray(/(\ude0a)😊(?<=\1)/.exec("\ude0a😊"), ["\ude0a😊", "\ude0a"]);
