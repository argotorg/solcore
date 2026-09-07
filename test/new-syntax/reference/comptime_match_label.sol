function classify(x: word) returns (word) {
  match (x) {
case comptime 1 {
return 1;
}
default {
return 0;
}
}
}
