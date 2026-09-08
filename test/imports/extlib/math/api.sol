import internals.add;
import lib.util;

export {sum};

function sum(x: word) returns (word) {
  return add.inc(x) + util.offset();
}
