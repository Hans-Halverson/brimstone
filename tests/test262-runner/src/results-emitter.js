'use strict';

const EventEmitter = require('events');

class ResultsEmitter extends EventEmitter {
  constructor(subject) {
    super();

    let started = false;

    const next = test => {
      if (!started) {
        this.emit('start');
        started = true;
      }

      if (test.result.pass) {
        this.emit('pass', test);
      } else {
        this.emit('fail', test);
      }

      this.emit('test end', test);
    };

    const complete = () => {
      this.emit('end');
    };

    const error = (error) => {
      this.emit('error', error);
    };

    subject.subscribe({
      complete, error, next
    });
  }
}

module.exports = ResultsEmitter;
