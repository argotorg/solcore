data Option(a) = None | Some(a);

function join(mmx: Option(Option(word))) -> Option(word) {
  match mmx {
  | .Some(.Some(x)) => return .Some(x);
  | _ => return .None;
  }
}

function main() -> word {
  match join(.Some(.Some(9))) {
  | .Some(v) => return v;
  | .None => return 0;
  }
}
