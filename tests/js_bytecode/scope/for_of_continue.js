function cotinueToFinallyWithScopeRestore() {
  for (const x of []) {
    const y = 1;

    if (2) {
      // Jump to finally with 1 scope pop
      continue;
    }

    {
      const z = 3;

      if (4)  {
        // Jump to same finally branch as above
        continue;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}

function continueToFinallyNoScopeRestore() {
  for (var x of []) {
    const y = 1;

    if (2) {
      // Jump to finally with no scope pops
      continue;
    }

    {
      const z = 3;

      if (4)  {
        // Jump to same finally branch as above
        continue;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}
