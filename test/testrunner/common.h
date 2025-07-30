#pragma once

#include <boost/multiprecision/cpp_int.hpp>

#include "Exceptions.h"

namespace solidity
{
using bytes = std::vector<uint8_t>;
using bigint = boost::multiprecision::number<boost::multiprecision::cpp_int_backend<>>;
using u256 = boost::multiprecision::number<boost::multiprecision::cpp_int_backend<256, 256, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void>>;
using s256 = boost::multiprecision::number<boost::multiprecision::cpp_int_backend<256, 256, boost::multiprecision::signed_magnitude, boost::multiprecision::unchecked, void>>;

template <class T, class Out>
void toBigEndian(T _val, Out& o_out)
{
    for (auto i = o_out.size(); i != 0; _val >>= 8, i--)
    {
        T v = _val & (T)0xff;
        o_out[i - 1] = static_cast<typename Out::value_type>(static_cast<uint8_t>(v));
    }
}

inline bytes toBigEndian(u256 _val) { bytes ret(32); toBigEndian(_val, ret); return ret; }

static auto const* upperHexChars = "0123456789ABCDEF";
static auto const* lowerHexChars = "0123456789abcdef";
enum class WhenError
{
	DontThrow = 0,
	Throw = 1,
};
enum class HexPrefix
{
	DontAdd = 0,
	Add = 1,
};

enum class HexCase
{
	Lower = 0,
	Upper = 1,
	Mixed = 2,
};
/// Convert a single byte to a string of hex characters (of length two),
/// optionally with uppercase hex letters.
inline std::string toHex(uint8_t _data, HexCase _case = HexCase::Lower)
{
	solRequire(_case != HexCase::Mixed, util::BadHexCase, "Mixed case can only be used for byte arrays.");

	char const* chars = _case == HexCase::Upper ? upperHexChars : lowerHexChars;

	return std::string{
		chars[(unsigned(_data) / 16) & 0xf],
		chars[unsigned(_data) & 0xf]
	};
}

/// Convert a series of bytes to the corresponding string of hex duplets,
/// optionally with "0x" prefix and with uppercase hex letters.
inline std::string toHex(bytes const& _data, HexPrefix _prefix = HexPrefix::DontAdd, HexCase _case = HexCase::Lower)
{
	std::string ret(_data.size() * 2 + (_prefix == HexPrefix::Add ? 2 : 0), 0);

	size_t i = 0;
	if (_prefix == HexPrefix::Add)
	{
		ret[i++] = '0';
		ret[i++] = 'x';
	}

	// Mixed case will be handled inside the loop.
	char const* chars = _case == HexCase::Upper ? upperHexChars : lowerHexChars;
	size_t rix = _data.size() - 1;
	for (uint8_t c: _data)
	{
		// switch hex case every four hexchars
		if (_case == HexCase::Mixed)
			chars = (rix-- & 2) == 0 ? lowerHexChars : upperHexChars;

		ret[i++] = chars[(static_cast<size_t>(c) >> 4ul) & 0xfu];
		ret[i++] = chars[c & 0xfu];
	}
	solRequire(i == ret.size(), util::Exception, "");

	return ret;
}
inline int fromHex(char _i, WhenError _throw)
{
	if (_i >= '0' && _i <= '9')
		return _i - '0';
	if (_i >= 'a' && _i <= 'f')
		return _i - 'a' + 10;
	if (_i >= 'A' && _i <= 'F')
		return _i - 'A' + 10;
	if (_throw == WhenError::Throw)
		solThrow(util::BadHexCharacter, std::to_string(_i));
	else
		return -1;
}
inline bytes fromHex(std::string const& _s, WhenError _throw = WhenError::DontThrow)
{
	if (_s.empty())
		return {};

	unsigned s = (_s.size() >= 2 && _s[0] == '0' && _s[1] == 'x') ? 2 : 0;
	std::vector<uint8_t> ret;
	ret.reserve((_s.size() - s + 1) / 2);

	if (_s.size() % 2)
	{
		int h = fromHex(_s[s++], _throw);
		if (h != -1)
			ret.push_back(static_cast<uint8_t>(h));
		else
			return bytes();
	}
	for (unsigned i = s; i < _s.size(); i += 2)
	{
		int h = fromHex(_s[i], _throw);
		int l = fromHex(_s[i + 1], _throw);
		if (h != -1 && l != -1)
			ret.push_back(static_cast<uint8_t>(h * 16 + l));
		else
			return bytes();
	}
	return ret;
}

/// Concatenate the contents of a container onto a vector
template <class T, class U> std::vector<T>& operator+=(std::vector<T>& _a, U& _b)
{
	for (auto const& i: _b)
		_a.push_back(T(i));
	return _a;
}
/// Concatenate the contents of a container onto a vector, move variant.
template <class T, class U> std::vector<T>& operator+=(std::vector<T>& _a, U&& _b)
{
	std::move(_b.begin(), _b.end(), std::back_inserter(_a));
	return _a;
}

/// Concatenate two vectors of elements.
template <class T>
std::vector<T> operator+(std::vector<T> const& _a, std::vector<T> const& _b)
{
	std::vector<T> ret(_a);
	ret += _b;
	return ret;
}

/// Concatenate two vectors of elements, moving them.
template <class T>
std::vector<T> operator+(std::vector<T>&& _a, std::vector<T>&& _b)
{
	std::vector<T> ret(std::move(_a));
	assert(&_a != &_b);
	ret += std::move(_b);
	return ret;
}
}
