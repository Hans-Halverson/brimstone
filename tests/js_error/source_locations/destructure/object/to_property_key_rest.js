var poisoned = { toString() { throw new Error() } };

var { [poisoned]: a, ...b } = {};