function simpleBreak() {
  l1: {
    const x = 0;
  
    if (1) {
      // 1 scope pop
      break l1;
    }

    function inner() {
      return x;
    }
  }

  2;
}

function nestedBreaks() {
  l1: {
    const x = 0;
    
    if (1) {
      // 1 scope pop
      break l1;
    }

    l2: {
      const y = 1;

      if (1) {
        // 2 scope pops
        break l1;
      } else if (2) {
        // 1 scope pop
        break l2;
      }

      l3: {
        const z = 2;

        if (1) {
          // 3 scope pops
          break l1;
        } else if (2) {
          // 2 scope pops
          break l2;
        } else if (3) {
          // 1 scope pop
          break l3;
        }

        function inner() {
          return x + y + z;
        }
      }
    }
  }
}

function noCapturesInBreak() {
  const x = 0;

  l1: {
    if (1) {
      break l1;
    }

    function inner() {
      return x;
    }
  }

  2;
}