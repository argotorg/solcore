/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0
/**
 * EVM versioning.
 */

#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <string>

namespace solidity::evmasm
{
/// Virtual machine bytecode instruction. Forward declared from libevmasm/Instruction.h
enum class Instruction: uint8_t;
}

namespace solidity::langutil
{

/**
 * A version specifier of the EVM we want to compile to.
 * Defaults to the latest version deployed on Ethereum Mainnet at the time of compiler release.
 */
class EVMVersion
{
public:
	EVMVersion() = default;

	static EVMVersion current() { return {currentVersion}; }

	static EVMVersion constexpr homestead() { return {Version::Homestead}; }
	static EVMVersion constexpr tangerineWhistle() { return {Version::TangerineWhistle}; }
	static EVMVersion constexpr spuriousDragon() { return {Version::SpuriousDragon}; }
	static EVMVersion constexpr byzantium() { return {Version::Byzantium}; }
	static EVMVersion constexpr constantinople() { return {Version::Constantinople}; }
	static EVMVersion constexpr petersburg() { return {Version::Petersburg}; }
	static EVMVersion constexpr istanbul() { return {Version::Istanbul}; }
	static EVMVersion constexpr berlin() { return {Version::Berlin}; }
	static EVMVersion constexpr london() { return {Version::London}; }
	static EVMVersion constexpr paris() { return {Version::Paris}; }
	static EVMVersion constexpr shanghai() { return {Version::Shanghai}; }
	static EVMVersion constexpr cancun() { return {Version::Cancun}; }
	static EVMVersion constexpr prague() { return {Version::Prague}; }
	static EVMVersion constexpr osaka() { return {Version::Osaka}; }

	static auto constexpr allVersions() {
		return std::array{
			homestead(),
			tangerineWhistle(),
			spuriousDragon(),
			byzantium(),
			constantinople(),
			petersburg(),
			istanbul(),
			berlin(),
			london(),
			paris(),
			shanghai(),
			cancun(),
			prague(),
			osaka(),
		};
	}

	static auto constexpr allEOFVersions()
	{
		return std::array{
			std::optional<uint8_t>(),
			std::make_optional<uint8_t>(1)
		};
	}

	static std::optional<EVMVersion> fromString(std::string const& _version)
	{
		for (auto const& v: allVersions())
			if (_version == v.name())
				return v;
		return std::nullopt;
	}

	static EVMVersion firstWithEOF() { return {Version::Osaka}; }

	bool isExperimental() const {
		return m_version > currentVersion;
	}

	auto operator<=>(EVMVersion const&) const = default;

	std::string name() const
	{
		switch (m_version)
		{
		case Version::Homestead: return "homestead";
		case Version::TangerineWhistle: return "tangerineWhistle";
		case Version::SpuriousDragon: return "spuriousDragon";
		case Version::Byzantium: return "byzantium";
		case Version::Constantinople: return "constantinople";
		case Version::Petersburg: return "petersburg";
		case Version::Istanbul: return "istanbul";
		case Version::Berlin: return "berlin";
		case Version::London: return "london";
		case Version::Paris: return "paris";
		case Version::Shanghai: return "shanghai";
		case Version::Cancun: return "cancun";
		case Version::Prague: return "prague";
		case Version::Osaka: return "osaka";
		}
	}

	/// Has the RETURNDATACOPY and RETURNDATASIZE opcodes.
	bool supportsReturndata() const { return *this >= byzantium(); }
	bool hasStaticCall() const { return *this >= byzantium(); }
	bool hasBitwiseShifting() const { return *this >= constantinople(); }
	bool hasCreate2() const { return *this >= constantinople(); }
	bool hasExtCodeHash() const { return *this >= constantinople(); }
	bool hasChainID() const { return *this >= istanbul(); }
	bool hasSelfBalance() const { return *this >= istanbul(); }
	bool hasBaseFee() const { return *this >= london(); }
	bool hasBlobBaseFee() const { return *this >= cancun(); }
	bool hasPrevRandao() const { return *this >= paris(); }
	bool hasPush0() const { return *this >= shanghai(); }
	bool hasBlobHash() const { return *this >= cancun(); }
	bool hasMcopy() const { return *this >= cancun(); }
	bool supportsTransientStorage() const { return *this >= cancun(); }
	bool supportsEOF() const { return *this >= firstWithEOF(); }

	bool hasOpcode(evmasm::Instruction _opcode, std::optional<uint8_t> _eofVersion) const;

	/// Whether we have to retain the costs for the call opcode itself (false),
	/// or whether we can just forward easily all remaining gas (true).
	bool canOverchargeGasForCall() const { return *this >= tangerineWhistle(); }

private:
	enum class Version {
		Homestead,
		TangerineWhistle,
		SpuriousDragon,
		Byzantium,
		Constantinople,
		Petersburg,
		Istanbul,
		Berlin,
		London,
		Paris,
		Shanghai,
		Cancun,
		Prague,
		Osaka,
	};
	static auto constexpr currentVersion = Version::Prague;

	constexpr EVMVersion(Version _version): m_version(_version) {}

	Version m_version = currentVersion;
};

}
