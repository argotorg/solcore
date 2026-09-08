import traits;

#[derive(traits.Marker)]
enum Tagged {}

contract QualifiedDerive {
  function main(value: word) returns (word) { return value; }
}
