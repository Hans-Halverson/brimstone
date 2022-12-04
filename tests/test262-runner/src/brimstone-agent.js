const ConsoleAgent = require("eshost/lib/ConsoleAgent.js");
const ErrorParser = require("eshost/lib/parse-error.js");

class BrimstoneAgent extends ConsoleAgent {
  parseError(str) {
    const err = ErrorParser.parse(str);
    if (!err) {
      return err;
    }

    // Strip out filename
    err.message = err.message.replace(/^.*\.js/, "Error: <temporary>.js");

    return err;
  }
}

module.exports = BrimstoneAgent;