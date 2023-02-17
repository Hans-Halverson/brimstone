// Locations where in is still allowed in for loops

for (var {[a in b]: c} in y) {}
for ({[a in b]: c} in y) {}

for (var {a = b in c} in y) {}
for ({a = b in c} in y) {}

for (var [a = b in c] in y) {}
for ([a = b in c] in y) {}
