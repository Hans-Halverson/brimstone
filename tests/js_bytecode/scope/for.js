function startScopeNoTDZInOuter() {
  for (const x = 1; x < 10; x++) {
    x + inner;
    const inner = 0;
  }

  for (let y = 1; y < 10; y++) {
    y + inner;
    const inner = 0;
  }
}

function startScopeTDZInOuter() {
  for (const x = x; x < 10; x++) {
    1;
    inner;
    2;
    const inner = 0;
    3;
  }
}