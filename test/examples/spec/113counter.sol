import StorageLib;

/*
contract Counter {
  counter : word;

  function main() -> word {
    counter = add(counter, 1);
    return counter;
  }
}
*/

enum counter_sel { counter_sel }
impl CStructField<StructField<ContractStorage<()>, counter_sel>, word, ()> {}

contract Counter {
   function main () public returns (word) {
      let counter_map /*: MemberAccessProxy(ContractStorage(()), counter_sel, ()) */ = MemberAccessProxy(ContractStorage(()), counter_sel);
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(()), counter_sel)), add(rval(counter_map), 1));
      return rval(counter_map);
   }
}
