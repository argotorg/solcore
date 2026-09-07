import {*} from glob_amb_a hiding {shared};
import {*} from glob_amb_b;

function main(x: word) returns (word) {
  return shared(x);
}
