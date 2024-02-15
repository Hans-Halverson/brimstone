function forCaptureAndInnerCaptureWithContinue() {
  for (let x = 0; true;) {
    const y = 1;

    if (2) {
      // 1 scope pop
      continue;
    }

    {
      const z = 3;

      if (4)  {
        // 2 scope pops
        continue;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}

function forCaptureNoInnerCaptureWithBreak() {
  for (let x = 0; true;) {
    if (2) {
      // 0 scope pops
      continue;
    }

    {
      const y = 3;

      if (4)  {
        // 1 scope pops
        continue;
      }

      function inner() {
        return x + y;
      }
    }
  }
}

function noForCaptureButInnerCaptureWithContinue() {
  for (var x = 0; true;) {
    const y = 1;

    if (2) {
      // 1 scope pops
      continue;
    }

    {
      const z = 3;

      if (4)  {
        // 2 scope pop
        continue;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}

function noForCaptureOrInnerCaptureWithContinue() {
  for (var x = 0; true;) {
    var y = 1;

    if (2) {
      // 0 scope pops
      continue;
    }

    {
      const z = 3;

      if (4)  {
        // 1 scope pop
        continue;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}