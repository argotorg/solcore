enum Food { Curry, Beans, Other }
enum CFood { Red(Food), Green(Food), Nocolor }



  function fromEnum(x : CFood) returns (word) {
     match (x ) {
       case CFood.Red(Food.Curry) { return 1;
       } case CFood.Green(Food.Beans) { return 42;
       } default { return 3;
     } }
  }


contract FoodContract {
  function id(x : CFood) public returns (CFood) {
    return(x);
  }

  function main() public returns (word) {
  return fromEnum(id(CFood.Green(Food.Beans)));
  }
}
