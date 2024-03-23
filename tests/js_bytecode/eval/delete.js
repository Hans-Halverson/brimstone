// OPTIONS: --run

eval(`
  "sloppy direct eval";
  var x = 1;
  function f() {};

  delete x;
  delete f;
`);

eval?.(`
  "sloppy indirect eval";
  var x = 1;
  function f() {};

  delete x;
  delete f;
`);
