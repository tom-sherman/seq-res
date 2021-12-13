module.exports.fromSeq = seq => ({
  next() {
    let value = seq();

    if (!value) {
      return {
        value: undefined,
        done: true
      };
    }

    seq = value._1;

    return {
      value: value._0,
      done: false
    };
  },

  [Symbol.iterator]() { return this; }
});
