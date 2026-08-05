import std.{*};
import std.dispatch.{*};
import std.eip7951.{p256verify};

// Exercises the P256VERIFY (secp256r1) precompile at address 0x100, introduced
// by EIP-7951, through the std `p256verify` helper. It returns true for a valid
// signature and false for an invalid one.
contract P256Test {
    constructor() {}

    public function verifyValid() -> bool {
        return p256verify(
            bytes32(0xabcdef00112233445566778899aabbccddeeff00112233445566778899aabbcc),
            bytes32(0xa29295460e251beea1bdc9b84b2f3fe8e3a3e4d872baa3c55b78c9e448190ea9),
            bytes32(0x2d854575b092b3732d3d73c8414bda17f907776894cff2e8e25e733d200f3f5c),
            bytes32(0x6079df2480f92e4cf526c08e32ab82aed6599fddb777a039612fe7c9ef0247ba),
            bytes32(0xe2b4793e7c77585508c4780e4e53a36deefbb3548f4380ee6df04863cfc54c2d)
        );
    }

    public function verifyInvalid() -> bool {
        return p256verify(
            bytes32(0xabcdef00112233445566778899aabbccddeeff00112233445566778899aabbcd),
            bytes32(0xa29295460e251beea1bdc9b84b2f3fe8e3a3e4d872baa3c55b78c9e448190ea9),
            bytes32(0x2d854575b092b3732d3d73c8414bda17f907776894cff2e8e25e733d200f3f5c),
            bytes32(0x6079df2480f92e4cf526c08e32ab82aed6599fddb777a039612fe7c9ef0247ba),
            bytes32(0xe2b4793e7c77585508c4780e4e53a36deefbb3548f4380ee6df04863cfc54c2d)
        );
    }
}
