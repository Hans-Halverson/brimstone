/*---
description: Bug where lexer lookahead was incorrect for two-byte strings in unicode mode.
---*/

assert(new RegExp('☺\\w', 'u').test('☺a'));
assert(new RegExp("☺[\\w--\\d]", "v").test('☺a'));