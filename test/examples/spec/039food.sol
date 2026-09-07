
enum Food { Curry, Beans, Other }
enum CFood { Red(Food), Green(Food), Nocolor }




  function fromEnum(x : Food) returns (word) {
     match (x ) {
       case Food.Curry { return 1;
       } case Food.Beans { return 42;
       } case Food.Other { return 3;
     } }
  }


contract FoodContract {
  function eat(x : CFood) public returns (Food) {
    match (x ) {
       case CFood.Red(f) { return f;
       } case CFood.Green(f) { return f;
       } default { return Food.Other;
    } }
  }

  function main() public returns (word) {
  return fromEnum(eat(CFood.Green(Food.Beans)));
  }
}
