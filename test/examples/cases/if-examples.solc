function toBool(x : word) returns (bool) {
  match (x ) {
  case 0 { return false;
  } default { return true;
  } }
}

function gt(x : word, y : word) returns (bool) {
  let res : word;
  assembly {
    res := gt(x,y)
  }
  return toBool(res);
}

function max(x : word, y : word) returns (word) {
  let res : word;
  if (gt(x,y)) {
    res = x;
  } else {
    res = y;
  }
  return res;
}

function not(x:bool) returns (bool) {
  if (x) { return false; }  else { return true; }
}

function foo(x : word) returns (bool) {
  if (gt(x,0)) {
    return true;
  } else {
    return false;
  }
}


contract IfExamples {
	 function main() public returns (word) {
	   return (( not(foo(42)) ? 0 : 1));
	 }
}
