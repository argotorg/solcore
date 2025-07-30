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
/** @file FixedHash.h
 * @author Gav Wood <i@gavwood.com>
 * @date 2014
 *
 * The FixedHash fixed-size "hash" container type.
 */

#pragma once

#include "common.h"

#include <boost/functional/hash.hpp>
#include <boost/io/ios_state.hpp>

#include <array>
#include <cstdint>
#include <algorithm>
#include <boost/multiprecision/number.hpp>

/**
 * A modifiable reference to an existing object or vector in memory.
 */
template <class T>
class vector_ref
{
public:
    using value_type = T;
    using element_type = T;
    using mutable_value_type = typename std::conditional<std::is_const<T>::value, typename std::remove_const<T>::type, T>::type;
    using string_type = typename std::conditional<std::is_const<T>::value, std::string const, std::string>::type;
    using vector_type = typename std::conditional<std::is_const<T>::value, std::vector<typename std::remove_const<T>::type> const, std::vector<T>>::type;
    using iterator = T*;
    using const_iterator = T const*;

    static_assert(std::is_standard_layout_v<value_type> && std::is_trivial_v<value_type>, "vector_ref can only be used with PODs due to its low-level treatment of data.");

    vector_ref(): m_data(nullptr), m_count(0) {}
    /// Creates a new vector_ref to point to @a _count elements starting at @a _data.
    vector_ref(T* _data, size_t _count): m_data(_data), m_count(_count) {}
    /// Creates a new vector_ref pointing to the data part of a string (given as pointer).
    vector_ref(string_type* _data): m_data(reinterpret_cast<T*>(_data->data())), m_count(_data->size() / sizeof(T)) {}
    /// Creates a new vector_ref pointing to the data part of a string (given as reference).
    vector_ref(string_type& _data): vector_ref(&_data) {}
    /// Creates a new vector_ref pointing to the data part of a vector (given as pointer).
    vector_ref(vector_type* _data): m_data(_data->data()), m_count(_data->size()) {}
    explicit operator bool() const { return m_data && m_count; }

    std::vector<unsigned char> toBytes() const { return std::vector<unsigned char>(reinterpret_cast<unsigned char const*>(m_data), reinterpret_cast<unsigned char const*>(m_data) + m_count * sizeof(T)); }
    std::string toString() const { return std::string((char const*)m_data, ((char const*)m_data) + m_count * sizeof(T)); }

    operator vector_ref<T const>() const { return vector_ref<T const>(m_data, m_count); }

    T* data() const { return m_data; }
    /// @returns the number of elements referenced (not necessarily number of bytes).
    size_t size() const { return m_count; }
    bool empty() const { return !m_count; }
    /// @returns a new vector_ref which is a shifted and shortened view of the original data.
    /// If this goes out of bounds in any way, returns an empty vector_ref.
    /// If @a _count is ~size_t(0), extends the view to the end of the data.
    vector_ref<T> cropped(size_t _begin, size_t _count) const { if (m_data && _begin <= m_count && _count <= m_count && _begin + _count <= m_count) return vector_ref<T>(m_data + _begin, _count == ~size_t(0) ? m_count - _begin : _count); else return vector_ref<T>(); }
    /// @returns a new vector_ref which is a shifted view of the original data (not going beyond it).
    vector_ref<T> cropped(size_t _begin) const { if (m_data && _begin <= m_count) return vector_ref<T>(m_data + _begin, m_count - _begin); else return vector_ref<T>(); }

    T* begin() { return m_data; }
    T* end() { return m_data + m_count; }
    T const* begin() const { return m_data; }
    T const* end() const { return m_data + m_count; }

    T& operator[](size_t _i) { assert(m_data); assert(_i < m_count); return m_data[_i]; }
    T const& operator[](size_t _i) const { assert(m_data); assert(_i < m_count); return m_data[_i]; }

    bool operator==(vector_ref<T> const& _cmp) const { return m_data == _cmp.m_data && m_count == _cmp.m_count; }
    bool operator!=(vector_ref<T> const& _cmp) const { return !operator==(_cmp); }

    void reset() { m_data = nullptr; m_count = 0; }

private:
    T* m_data = nullptr;
    size_t m_count = 0;
};

using bytes = std::vector<uint8_t>;
using bytesRef = vector_ref<uint8_t>;
using bytesConstRef = vector_ref<uint8_t const>;
/// Fixed-size raw-byte array container type, with an API optimised for storing hashes.
/// Transparently converts to/from the corresponding arithmetic type; this will
/// assume the data contained in the hash is big-endian.
template <unsigned N>
class FixedHash
{
public:
	/// The corresponding arithmetic type.
	using Arith = boost::multiprecision::number<boost::multiprecision::cpp_int_backend<N * 8, N * 8, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void>>;

	/// The size of the container.
	enum { size = N };
	static_assert(N != 0);

	/// Method to convert from a string.
	enum ConstructFromStringType { FromHex, FromBinary };

	/// Method to convert from a string.
	enum ConstructFromHashType { AlignLeft, AlignRight, FailIfDifferent };

	/// Construct an empty hash.
	explicit FixedHash() { m_data.fill(0); }

	/// Construct from another hash, filling with zeroes or cropping as necessary.
	template <unsigned M> explicit FixedHash(FixedHash<M> const& _h, ConstructFromHashType _t)
	{
		m_data.fill(0);
		unsigned c = std::min(M, N);
		for (unsigned i = 0; i < c; ++i)
			m_data[_t == AlignRight ? N - 1 - i : i] = _h[_t == AlignRight ? M - 1 - i : i];
	}

	/// Convert from the corresponding arithmetic type.
	FixedHash(Arith const& _arith) { solidity::toBigEndian(_arith, m_data); }

	/// Explicitly construct, copying from a byte array.
	explicit FixedHash(bytes const& _array, ConstructFromHashType _sizeMismatchBehavior = FailIfDifferent)
	{
		if (_array.size() == N)
			memcpy(m_data.data(), _array.data(), _array.size());
		else
		{
			m_data.fill(0);
			if (_sizeMismatchBehavior != FailIfDifferent)
			{
				auto bytesToCopy = std::min<size_t>(_array.size(), N);
				for (size_t i = 0; i < bytesToCopy; ++i)
					if (_sizeMismatchBehavior == AlignRight)
						m_data[N - 1 - i] = _array[_array.size() - 1 - i];
					else
						m_data[i] = _array[i];
			}
		}
	}

	/// Explicitly construct, copying from a byte array.
	explicit FixedHash(bytesConstRef _b, ConstructFromHashType _t = FailIfDifferent)
	{
		if (_b.size() == N)
			memcpy(m_data.data(), _b.data(), std::min<size_t>(_b.size(), N));
		else
		{
			m_data.fill(0);
			if (_t != FailIfDifferent)
			{
				auto c = std::min<size_t>(_b.size(), N);
				for (size_t i = 0; i < c; ++i)
					if (_t == AlignRight)
						m_data[N - 1 - i] = _b[_b.size() - 1 - i];
					else
						m_data[i] = _b[i];
			}
		}
	}
    enum class WhenError
    {
        DontThrow = 0,
        Throw = 1,
    };
    inline bytes asBytes(std::string const& _b)
    {
        return bytes((uint8_t const*)_b.data(), (uint8_t const*)(_b.data() + _b.size()));
    }
    int fromHex(char _i, WhenError _throw) const
    {
        if (_i >= '0' && _i <= '9')
            return _i - '0';
        if (_i >= 'a' && _i <= 'f')
            return _i - 'a' + 10;
        if (_i >= 'A' && _i <= 'F')
            return _i - 'A' + 10;
        if (_throw == WhenError::Throw)
            throw std::runtime_error("");
        else
            return -1;
    }
    bytes fromHex(std::string const& _s, WhenError _throw) const
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
	/// Explicitly construct, copying from a string.
	explicit FixedHash(std::string const& _s, ConstructFromStringType _t = FromHex, ConstructFromHashType _ht = FailIfDifferent):
		FixedHash(_t == FromHex ? fromHex(_s, WhenError::Throw) : asBytes(_s), _ht)
	{}
    template <class T, class In>
    inline T fromBigEndian(In const& _bytes) const
    {
        T ret = (T)0;
        for (auto i: _bytes)
            ret = (T)((ret << 8) | (uint8_t)(typename std::make_unsigned<typename In::value_type>::type)i);
        return ret;
    }
	/// Convert to arithmetic type.
	operator Arith() const { return fromBigEndian<Arith>(m_data); }

	// The obvious comparison operators.
	bool operator==(FixedHash const& _c) const { return m_data == _c.m_data; }
	bool operator!=(FixedHash const& _c) const { return m_data != _c.m_data; }
	/// Required to sort objects of this type or use them as map keys.
	bool operator<(FixedHash const& _c) const {
		for (unsigned i = 0; i < N; ++i)
		{
			if (m_data[i] < _c.m_data[i])
				return true;
			else if (m_data[i] > _c.m_data[i])
				return false;
		}
		return false;
	}

	/// @returns a particular byte from the hash.
	uint8_t& operator[](unsigned _i) { return m_data[_i]; }
	/// @returns a particular byte from the hash.
	uint8_t operator[](unsigned _i) const { return m_data[_i]; }

	/// @returns the hash as a user-readable hex string.
	std::string hex() const { return toHex(asBytes()); }

	/// @returns a mutable byte vector_ref to the object's data.
	bytesRef ref() { return bytesRef(m_data.data(), N); }

	/// @returns a constant byte vector_ref to the object's data.
	bytesConstRef ref() const { return bytesConstRef(m_data.data(), N); }

	/// @returns a mutable byte pointer to the object's data.
	uint8_t* data() { return m_data.data(); }

	/// @returns a constant byte pointer to the object's data.
	uint8_t const* data() const { return m_data.data(); }

	/// @returns a copy of the object's data as a byte vector.
	bytes asBytes() const { return bytes(data(), data() + N); }

private:
	std::array<uint8_t, N> m_data; ///< The binary data.
};

/// Stream I/O for the FixedHash class.
template <unsigned N>
inline std::ostream& operator<<(std::ostream& _out, FixedHash<N> const& _h)
{
	boost::io::ios_all_saver guard(_out);
	_out << std::noshowbase << std::hex << std::setfill('0');
	for (unsigned i = 0; i < N; ++i)
		_out << std::setw(2) << (int)_h[i];
	_out << std::dec;
	return _out;
}

// Common types of FixedHash.
using h256 = FixedHash<32>;
using h160 = FixedHash<20>;
