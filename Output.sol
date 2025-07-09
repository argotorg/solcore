// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.23;
import {console,Script} from "lib/stdlib.sol";
contract C is Script {
  function run() public view {
    console.log("RESULT --> ", wrapper());
  }
  
  function wrapper ()  public pure returns (uint256 _wrapresult) {
    assembly {
      function usr$Assign_assign$memoryRefLaJ$uint (_v0, _v1) {
        let _v2
        _v2 := usr$Typedef_rep$memoryRefLaJ$uint(_v0)
        usr$MemoryType_store$uint(_v2, _v1)
      }
      function usr$Assign_assign$memoryRefLaJ$word (_v3, y) {
        let _v4
        _v4 := usr$Typedef_rep$memoryRefLaJ$word(_v3)
        usr$MemoryType_store$word(_v4, y)
      }
      function usr$Assign_assign$refLaJ$memoryLwordJ (_v5, _v6) { leave }
      function usr$Assign_assign$refLaJ$word (_v7, r) { leave }
      function usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_x_sel_word (_v8) -> _result {
        let ptr
        let _v9
        _v9 := usr$memberAccessD1$memoryLSJ_x_sel(_v8)
        let _v10
        _v10 := usr$Typedef_rep$memoryLaJ$S(_v9)
        ptr := _v10
        let size
        let _v11
        _v11 := usr$MemorySize_size$unit()
        size := _v11
        ptr := add(ptr, size)
        let _v12
        _v12 := usr$Typedef_abs$word(ptr)
        _result := _v12
        leave
      }
      function usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_y_sel_uint (_v13) -> _result {
        let ptr
        let _v14
        _v14 := usr$memberAccessD1$memoryLSJ_y_sel(_v13)
        let _v15
        _v15 := usr$Typedef_rep$memoryLaJ$S(_v14)
        ptr := _v15
        let size
        let _v16
        _v16 := usr$MemorySize_size$unit()
        size := _v16
        ptr := add(ptr, size)
        let _v17
        _v17 := usr$Typedef_abs$word(ptr)
        _result := _v17
        leave
      }
      function usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_z_sel_word (_v18) -> _result {
        let ptr
        let _v19
        _v19 := usr$memberAccessD1$memoryLSJ_z_sel(_v18)
        let _v20
        _v20 := usr$Typedef_rep$memoryLaJ$S(_v19)
        ptr := _v20
        let size
        let _v21
        _v21 := usr$MemorySize_size$unit()
        size := _v21
        ptr := add(ptr, size)
        let _v22
        _v22 := usr$Typedef_abs$word(ptr)
        _result := _v22
        leave
      }
      function usr$MemorySize_size$unit () -> _result {
        _result := 0
        leave
      }
      function usr$MemoryType_load$word (ptr) -> _result {
        let r
        r := mload(ptr)
        _result := r
        leave
      }
      function usr$MemoryType_store$uint (ptr, _v23) {
        let _v24
        _v24 := usr$Typedef_rep$uint(_v23)
        usr$MemoryType_store$word(ptr, _v24)
        leave
      }
      function usr$MemoryType_store$word (ptr, value) {
        mstore(ptr, value)
      }
      function usr$RValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_x_sel_word (_v25) -> _result {
        let ptr
        let _v26
        _v26 := usr$memberAccessD1$memoryLSJ_x_sel(_v25)
        let _v27
        _v27 := usr$Typedef_rep$memoryLaJ$S(_v26)
        ptr := _v27
        let size
        let _v28
        _v28 := usr$MemorySize_size$unit()
        size := _v28
        ptr := add(ptr, size)
        let _v29
        _v29 := usr$Typedef_abs$word(ptr)
        let _v30
        _v30 := usr$MemoryType_load$word(_v29)
        _result := _v30
        leave
      }
      function usr$Typedef_abs$memoryLaJ$S (x) -> _result {
        _result := x
        leave
      }
      function usr$Typedef_abs$word (x) -> _result {
        _result := x
        leave
      }
      function usr$Typedef_rep$memoryLaJ$S (_v31) -> _result {
        let var_2
        var_2 := _v31
        _result := var_2
        leave
      }
      function usr$Typedef_rep$memoryRefLaJ$uint (_v32) -> _result {
        let var_2
        var_2 := _v32
        _result := var_2
        leave
      }
      function usr$Typedef_rep$memoryRefLaJ$word (_v33) -> _result {
        let var_2
        var_2 := _v33
        _result := var_2
        leave
      }
      function usr$Typedef_rep$uint (_v34) -> _result {
        let var_2
        var_2 := _v34
        _result := var_2
        leave
      }
      function usr$f () {
        let _v35
        let _v36
        usr$Assign_assign$refLaJ$memoryLwordJ(_v35, _v36)
      }
      function usr$g () {
        let _v37
        let _v38
        _v38 := usr$Typedef_abs$memoryLaJ$S(128)
        _v37 := _v38
        let y
        y := 42
        let _v39
        _v39 := 42
        let _v40
        _v40 := usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_x_sel_word(_v37)
        usr$Assign_assign$memoryRefLaJ$word(_v40, y)
        let _v41
        _v41 := usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_y_sel_uint(_v37)
        usr$Assign_assign$memoryRefLaJ$uint(_v41, _v39)
        let _v42
        _v42 := usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_z_sel_word(_v37)
        usr$Assign_assign$memoryRefLaJ$word(_v42, y)
        let _v43
        _v43 := usr$RValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_x_sel_word(_v37)
        usr$Assign_assign$refLaJ$word(y, _v43)
        let _v44
        _v44 := usr$LValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_z_sel_word(_v37)
        let _v45
        _v45 := usr$RValueMemberAccess_memberAccess$MemberAccessProxyLmemoryLaJ_bJ$S_x_sel_word(_v37)
        usr$Assign_assign$memoryRefLaJ$word(_v44, _v45)
      }
      function usr$main () -> _v46 {
        usr$f()
        usr$g()
      }
      function usr$memberAccessD1$memoryLSJ_x_sel (_v47) -> _result {
        let _v48
        _v48 := _v47
        _result := _v48
        leave
      }
      function usr$memberAccessD1$memoryLSJ_y_sel (_v49) -> _result {
        let _v50
        _v50 := _v49
        _result := _v50
        leave
      }
      function usr$memberAccessD1$memoryLSJ_z_sel (_v51) -> _result {
        let _v52
        _v52 := _v51
        _result := _v52
        leave
      }
      let _mainresult := usr$main()
      _wrapresult := _mainresult
    }
  }
}