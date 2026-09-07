function value() returns (word) {
  let result: word;
  assembly { result := 7 }
  return result;
}

function applyCallback(callback: function() returns (word)) returns (word) {
  return callback();
}

contract NullaryCallback {
  function main() returns (word) {
    return applyCallback(value);
  }
}
