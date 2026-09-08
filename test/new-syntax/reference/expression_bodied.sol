function zero() returns (word) {
  0
}

function apply<a, b>(f: function(a) returns (b), x: a) returns (b) {
  f(x)
}

function choose<a>(c: bool, a: a, b: a) returns (a) {
   c  ?  a  :  b
}

function keepThen(then: word) returns (word) {
  then
}
