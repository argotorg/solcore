contract CondExp {
   function main() public returns (word) {
     return
       ( ( true ? false : true
       )? ( false ? 1 : 2
       ): ( true ? 42 : 56));
   }
}