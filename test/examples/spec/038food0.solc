data Food = Curry | Beans | Other;
data CFood = Red(Food) | Green(Food) | Nocolor;



  function fromEnum(x : CFood) -> word {
     match x {
       | CFood.Red(Food.Curry) => return 1;
       | CFood.Green(Food.Beans) => return 42;
       | _ => return 3;
     }
  }


contract FoodContract {
  public function id(x : CFood) -> CFood {
    return(x);
  }

  public function main() -> word {
  return fromEnum(id(CFood.Green(Food.Beans)));
  }
}
