function notNeededDueToParameterName(arguments) {
  arguments;
}

function notNeededDueToLexicalName() {
  const arguments = 1;
  arguments;
}

function notNeededDueToLexicalNameAndParameterExpressionsNoUse(x = 1) {
  // Arguments use is not in parameter scope
  arguments;
  const arguments = 1;
}

function neededDueToLexicalNameAndParameterExpressionsWithUse(x = arguments) {
  const arguments = 1;
}

function neededWithVarName() {
  var arguments;
  arguments;
}

function neededWithVarFunctionName() {
  function arguments() {}
  arguments;
}

function notNeededSinceUseCaught() {
  {
    const arguments = 1;
    {
      arguments;
    }
  }
}

function neededDueToDirectEval() {
  "use strict";
  eval('');
}

function notNeededDueToIndirectEval() {
  eval?.('');
}