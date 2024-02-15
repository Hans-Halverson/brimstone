function basic() {
  var v1 = 1;

  function inner() {
    return v1 + 2;
  }
}

function multipleCaptures() {
  var v1 = 1;
  var v2 = 2;

  function inner() {
    return v1 + v2;
  }
}

function capturedLexical() {
  const c1 = 1;
  let l1 = 2;

  // Accessing requires tdz check in same scope
  c1 + l1;

  function inner() {
    return c1 + l1;
  }
}

function storingToCapture() {
  var v1 = 1;
  let l1 = 2;

  function inner() {
    v1 = 3;
    l1 = 4;
  }
}

var globalVar1 = 0;
var globalConst1 = 1;
var globalLet1 = 2;

function capturedGlobal() {
  // Accessing globals in global scope
  globalVar1 + globalConst1 + globalLet1;

  // Setting globals
  globalVar1 = 3;
  globalLet1 = 5;

  function inner1() {
    var v1 = 1;

    // Accessing globals in nested scope
    globalVar1 + globalConst1 + globalLet1;

    function inner2() {
      return v1 + globalVar1 + globalConst1 + globalLet1;
    }
  }
}

function nestedScopes() {
  var v1 = 1;
  
  function inner1() {
    var v2 = 2;

    v1 + v2;

    function inner2() { 
      var v3 = 3;
      
      v1 + v2 + v3;

      function inner3() {
        var v4 = 4;

        return v1 + v2 + v3 + v4;
      }
    }
  }
}

function arrowFunctionsCapture() {
  // Arrow functions capture
  var v1 = 1;
  (() => v1);

  // Function expressions capture
  var v2 = 2;
  (function () { return v2 })
}

function capturedVarsHoisted() {
  var v1 = 1;
  
  {
    var v2 = 2;
    {
      var v3 = 3;
    }
  }

  function inner() {
    return v1 + v2 + v3;
  }

  return v1 + v2 + v3;
}

function blockScopes() {
  var v1 = 1;
  {
    const v2 = 2;
    {
      const v3 = 3;
      {
        const v4 = 4;
        function inner() {
          return v1 + v2 + v3 + v4;
        }
      }
    }
  }
}

function popBetweenBlocks() {
  {
    const v1 = 1;
    function innner1() {
      return v1;
    }
  }

  {
    const v2 = 2;
    function inner2() {
      return v2;
    }
  }
}