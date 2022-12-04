'use strict';
const {Subject} = require('rxjs');
const BrimstoneAgent = require('./brimstone-agent.js');

const internal = new WeakMap();

class AgentPool extends Subject {
  constructor(agentCount, hostType, hostArguments, hostPath, options = { timeout: 10000 }) {
    super();

    this.options = options;
    this.agents = [];

    for (let i = 0; i < agentCount; i++) {
      const agent = new BrimstoneAgent({
        hostArguments,
        hostPath,
        shortName: '$262',
        transform: options.transform,
        out: options.tempDir,
      });

      agent.initialize()
      .then(agent => {
        this.agents.push(agent);
        this.next(agent);
      })
      .catch(error => displayErrorAndExit(error));
    }
  }

  destroy() {
    this.agents.forEach(agent => agent.destroy());
  }

  async runTest(record) {
    const [agent, test] = record;
    const {async = false, module = false} = test.attrs.flags || {};

    let result;
    let stopPromise;

    if (test.result) {
      result = Promise.resolve(test.result);
    } else {
      result = agent.evalScript(test, {async, module});
    }

    const timeout = setTimeout(() => {
      stopPromise = agent.stop();
    }, this.options.timeout);

    return result.then(result => {
      clearTimeout(timeout);
      this.next(agent);

      test.rawResult = result;
      test.compiled = agent.compile(test.contents);

      if (stopPromise) {
        test.rawResult.timeout = true;
        // wait for the host to stop, then return the test
        return stopPromise.then(() => test);
      }

      const doneError = result.stdout.match(/^Test262:AsyncTestFailure:(.*)$/gm);

      if (doneError) {
        const lastErrorString = doneError[doneError.length - 1];
        const [
          /* ignored */,
          name,
          message
        ] = lastErrorString.match(/Test262:AsyncTestFailure:([^:]+): (.*)/);

        test.rawResult.error = {
          name,
          message,
        };
      }
      return test;
    })
    .catch(error => displayErrorAndExit(error));
  }
}

function displayErrorAndExit(error) {
  console.error('Error running test: ');
  console.error(error);
  process.exit(1);
}

module.exports = AgentPool;
