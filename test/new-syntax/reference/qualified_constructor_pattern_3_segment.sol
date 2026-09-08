function main(x: mod.Type.Bool) returns (word) {
  match (x) {
case mod.Type.True {
return 1;
}
default {
return 0;
}
}
}
