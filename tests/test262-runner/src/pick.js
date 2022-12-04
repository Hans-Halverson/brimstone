'use strict';

module.exports = function(obj, keys) {
  return keys.reduce((accum, key) => {
    if (!key.includes('.')) {
      // Regular keys, just add them
      accum[key] = obj[key];
      return accum;
    }

    // Complex keys, let's split them out
    const [parent, child] = key.split('.');

    // No need to add if it's already adding the whole parent property.
    if (keys.includes(parent)) {
      return accum;
    }

    // Fail-safe bail from undefined property
    if (!obj[parent]) {
      return accum;
    }

    if (!accum[parent]) {
      accum[parent] = {};
    }

    accum[parent][child] = obj[parent][child];
    return accum;
  }, {});
};
