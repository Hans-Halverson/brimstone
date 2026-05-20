function breakToFinallyWithScopeRestore() {
  for (const x of []) {
    const y = 1;

    if (2) {
      // Jump to finally with 1 scope pop
      break;
    }

    {
      const z = 3;

      if (4)  {
        // Jump to same finally branch as above
        break;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}

function breakToFinallyNoScopeRestore() {
  for (var x of []) {
    const y = 1;

    if (2) {
      // Jump to finally with no scope pops
      break;
    }

    {
      const z = 3;

      if (4)  {
        // Jump to same finally branch as above
        break;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}
