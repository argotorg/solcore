import {*} from std;
import {*} from std.dispatch;
import {mstore} from std.opcodes;

contract C {
    function dirty_allocate() public returns (bytes memory) {
        mstore(get_free_memory() + 32, 0xdeadc0de);
        let ptr = allocate_memory(32 + 32);
        mstore(ptr, 32);
        return memory(ptr);
    }

    function clear_allocate() public returns (bytes memory) {
        mstore(get_free_memory() + 32, 0xdeadc0de);
        let ptr = allocate_zeroed_memory(32 + 32);
        mstore(ptr, 32);
        return memory(ptr);
    }
}
