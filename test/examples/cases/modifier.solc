contract C {
	public function add(x: word, y:word) -> word {
		let r : word;
		assembly {
			r := add(x, y)
		}
		return r;
	}

	// modifier pattern: wrap add with before/after code
	public function modifiedAdd(x : word, y : word) -> word {
		// before solidity placeholder
		let result = add(x, y); // Solidity's placeholder: _;
		// after solidity placeholder
		return result;
	}

	public function main() -> word {
		return modifiedAdd(2, 1);
	}
}
