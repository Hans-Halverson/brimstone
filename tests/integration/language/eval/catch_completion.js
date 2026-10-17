/*---
description: Completion values of try-catch statements.
---*/

assert.sameValue(eval("1; try { 2; throw 0 } catch (e) { }"), undefined);
assert.sameValue(eval("1; try { 2; throw 0 } catch { }"), undefined);
assert.sameValue(eval("1; try { 2; throw 0 } catch ({ x }) { }"), undefined);
assert.sameValue(eval("1; try { 2; throw 0 } catch (e) { var x; }"), undefined);
assert.sameValue(eval("1; try { 2; throw 0 } catch (e) { } finally { 3 }"), undefined);
assert.sameValue(eval("1; try { 2; throw 0 } catch (e) { 4 }"), 4);
assert.sameValue(eval("1; try { 2 } catch (e) { }"), 2);
