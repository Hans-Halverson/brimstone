// Print easily parseable results from a particular run of Octane
BenchmarkSuite.RunSuites({
  NotifyResult: function (name, result) {
    print("RESULT " + name + " " + result);
  },
  NotifyError: function (name, error) {
    print("ERROR " + name + " " + error);
  },
  NotifyScore: function (score) {
    print("SCORE " + score);
  },
});
