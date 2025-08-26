// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.23;
import {console,Script} from "lib/stdlib.sol";
contract Tiamat is Script {
  function run() public {
    console.log("RESULT --> ", wrapper());
  }
  
  function wrapper ()  public returns (uint256 _wrapresult) {
    assembly {
      function usr$Assign_assign$storageLk19J$word (_v0, r) {
        let _v1
        _v1 := usr$saddr$word(_v0)
        usr$StorageType_store$word(_v1, r)
      }
      function usr$LVA_acc$TIPLstorageLdictLb16_g16JJ_b16_g16J$address_dictLaddress_wordJ (_v2, _v3) -> _result {
        _result := 42
        leave
      }
      function usr$LVA_acc$TIPLstorageLdictLb16_g16JJ_b16_g16J$address_word (_v4, _v5) -> _result {
        _result := 42
        leave
      }
      function usr$RVA_acc$TIPLstorageLdictLq8_b9JJ_q8_b9J$address_word (_v6, _v7) -> _result {
        let addr
        let _v8
        _v8 := usr$LVA_acc$TIPLstorageLdictLb16_g16JJ_b16_g16J$address_word(_v6, _v7)
        let _v9
        _v9 := usr$saddr$word(_v8)
        addr := _v9
        let _v10
        _v10 := usr$StorageType_sload$word(addr)
        _result := _v10
        leave
      }
      function usr$StorageType_sload$word (ptr) -> _result {
        let r
        r := sload(ptr)
        _result := r
        leave
      }
      function usr$StorageType_store$word (ptr, value) {
        sstore(ptr, value)
      }
      function usr$getAllowance (_v11, _v12, _v13) -> _result {
        let _v14
        _v14 := usr$LVA_acc$TIPLstorageLdictLb16_g16JJ_b16_g16J$address_dictLaddress_wordJ(_v11, _v12)
        let _v15
        _v15 := usr$RVA_acc$TIPLstorageLdictLq8_b9JJ_q8_b9J$address_word(_v14, _v13)
        _result := _v15
        leave
      }
      function usr$main () -> _result {
        let _v16
        let _v17
        _v17 := 17
        usr$setAllowance(_v16, 1, 2, 666)
        let _v18
        _v18 := usr$getAllowance(_v16, 1, 2)
        _result := _v18
        leave
      }
      function usr$saddr$word (_v19) -> _result {
        let var_2
        var_2 := _v19
        _result := var_2
        leave
      }
      function usr$setAllowance (_v20, _v21, _v22, amt) {
        let _v23
        let _v24
        _v23 := _v20
        _v24 := _v21
        let _v25
        let _v26
        _v26 := usr$LVA_acc$TIPLstorageLdictLb16_g16JJ_b16_g16J$address_dictLaddress_wordJ(_v23, _v24)
        _v25 := _v26
        let _v27
        let _v28
        _v27 := _v25
        _v28 := _v22
        let _v29
        let _v30
        _v30 := usr$LVA_acc$TIPLstorageLdictLb16_g16JJ_b16_g16J$address_word(_v27, _v28)
        _v29 := _v30
        usr$Assign_assign$storageLk19J$word(_v29, amt)
      }
      let _mainresult := usr$main()
      _wrapresult := _mainresult
    }
  }
}