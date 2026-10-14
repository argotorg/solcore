// Transaction context (std gap): msg.sender. Should disappear once std
// provides transaction-context helpers.
import * from std;
import {caller} from std.opcodes;

export { sender };

// msg.sender: the CALLER opcode lifted from word into address.
function sender() returns (address) {
  return address(caller());
}
