for (var a of b) {}

for (var a in b) {}

for (let a of b) {}

for (const a of b) {}

for (a of b) {}

for (a in b) {}

async function foo() {
  for await (a of b) {}
  for await (async of b) {}
}

// Sequence expression allowed in for-in
for (var a in 1, 2, 3) {}