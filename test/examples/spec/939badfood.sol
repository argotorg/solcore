trait Enum<a> {
  function fromEnum(x : a) returns (word);
}

enum Food { Curry, Beans, Other }

impl Enum<Food> {
  function fromEnum(x : Food) returns (word) {
     match (x ) {
       case Food.Curry { return 1;
       } case Food.Beans { return 2;
       } case Food.Other { return 3;
     } }
  }
}

contract FoodContract {
  function main() public returns (word) {
        return Enum.fromEnum(Food.Beans);
  }
}
