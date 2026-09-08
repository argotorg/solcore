import {Generic, CloneLike, Marker as Tagged} from traits;

#[derive(CloneLike, Tagged)]
enum Box { Box(word) }

contract Derived {
  function main(value: Box) returns (Box) {
    return CloneLike.clone(value);
  }
}
