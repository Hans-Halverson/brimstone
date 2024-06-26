function forCaptureAndInnerCaptureWithBreak() {
  for (const x in {}) {
    const y = 1;

    if (2) {
      // 2 scope pops
      break;
    }

    {
      const z = 3;

      if (4)  {
        // 3 scope pops
        break;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}

function forCaptureNoInnerCaptureWithBreak() {
  for (const x in {}) {
    if (2) {
      // 1 scope pop
      break;
    }

    {
      const y = 3;

      if (4)  {
        // 2 scope pops
        break;
      }

      function inner() {
        return x + y;
      }
    }
  }
}

function noForCaptureButInnerCaptureWithBreak() {
  for (var x in {}) {
    const y = 1;

    if (2) {
      // 1 scope pop
      break;
    }

    {
      const z = 3;

      if (4)  {
        // 2 scope pops
        break;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}

function noForCaptureOrInnerCaptureWithBreak() {
  for (var x in {}) {
    var y = 1;

    if (2) {
      // 0 scope pops
      break;
    }

    {
      const z = 3;

      if (4)  {
        // 1 scope pop
        break;
      }

      function inner() {
        return x + y + z;
      }
    }
  }
}