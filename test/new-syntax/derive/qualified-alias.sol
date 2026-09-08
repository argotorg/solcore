import * as api from traits;

#[derive(api.Marker)]
enum Tagged {}

contract QualifiedAliasDerive {
  function main(value: word) returns (word) { return value; }
}
