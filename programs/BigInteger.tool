object BigInteger {
    def main() : Unit = {
    	println(new Computer().compute());
    }
}

class Computer {
	def compute() : String = {
		var a: BigInt;
		var b: BigInt;
		var res1: BigInt;
		var res2: BigInt;
		var res3: BigInt;
		var res4: BigInt;

		a = new BigInt().init(20);
		println(a.toString() + "! = " + new BigInt().factorial(a).toString());

		return "Finished";
	}
}

class BigInt {
	var digits: Int[];

	def init(num: Int): BigInt = {
		var numDigits: Int;
		var singleDigit: Int;
		var tempValue: Int;
		var i: Int;
		var lengthA: Int;

		if (num == 0) {
			digits = new Int[1];
			digits[0] = 0;
		}
		else {
			// Count the total number of digits
			i = 0;
			tempValue = num;
			while (!(tempValue == 0)) {
				tempValue = tempValue / 10;
				i = i + 1;
			}
			lengthA = i;
			digits = new Int[lengthA];
			// Fill the array
			i = 0;
			while (!(num == 0)) {
				tempValue = num / 10;
				singleDigit = num - tempValue * 10;
				num = tempValue;
				digits[i] = singleDigit;
				i = i + 1;
			}
		}

		return this;
	}

	def sum(other: BigInt) : BigInt = {
		var lengthA: Int;
		var lengthB: Int;
		var lengthRes: Int;
		var CDigits: Int[];
		var i: Int;
		var singleDigit: Int;
		var carry: Int;
		var firstAddend: Int;
		var secondAddend: Int;
		var tempSum: Int;
		var res: BigInt;
		var a: Int;

		lengthA = digits.length;
		lengthB = other.getLength();
		lengthRes = lengthA + lengthB;
		CDigits = new Int[lengthRes];

		i = 0;
		carry = 0;
		while (i < lengthRes) {
			if (i < lengthA) {
				firstAddend = digits[i];
			}
			else {
				firstAddend = 0;
			}
			if (i < lengthB) {
				secondAddend = other.getDigits()[i];
			}
			else {
				secondAddend = 0;
			}
			tempSum = firstAddend + secondAddend + carry;

			carry = tempSum / 10;
			singleDigit = tempSum - carry * 10;

			CDigits[i] = singleDigit;

			i = i + 1;
		}

		res = new BigInt().setDigits(CDigits);
		res = res.deleteZeros();

		return res;
	}

	def multiply(other: BigInt) : BigInt = {
		var digitsA: Int[];
		var digitsB: Int[];
		var lengthA: Int;
		var lengthB: Int;
		var i: Int;
		var j: Int;
		var singleDigit: Int;
		var carry: Int;
		var prod: Int;
		var tempMult: Int;
		var addend: BigInt;
		var tempAddend: BigInt;

		digitsA = digits;
		digitsB = other.getDigits();
		lengthA = digitsA.length;
		lengthB = digitsB.length;

		i = 0;
		addend = new BigInt().init(0);
		while (i < lengthA) {
			j = 0;
			carry = 0;
			while (j < lengthB) {
				prod = digitsA[i] * digitsB[j] + carry;
				carry = prod / 10;
				singleDigit = prod - carry * 10;
				tempAddend = new BigInt().init(singleDigit).multiplyForTenPow(i + j);
				addend = addend.sum(tempAddend);
				j = j + 1;
			}
			if (!(carry == 0)) {
				addend = addend.sum(new BigInt().init(carry).multiplyForTenPow(i + j));
			}
			i = i + 1;
		}

		return addend;
	}

	def squared() : BigInt = {
		return this.multiply(this);
	}

	def factorial(num: BigInt) : BigInt = {
		var res: BigInt;

		if ((num.getLength() == 1) && (num.getDigits()[0] == 1)) {
			res = new BigInt().init(1);
		}
		else {
			res = num.multiply(this.factorial(num.minusOne()));
		}
		return res;
	}

	def minusOne() : BigInt = {
		var i: Int;
		var res: BigInt;
		var resDigits: Int[];

		i = 0;
		resDigits = new Int[this.getLength()];
		while (i < this.getLength()) {
			resDigits[i] = digits[i];
			i = i + 1;
		}

		i = 0;
		while (resDigits[i] == 0) {
			i = i + 1;
		}
		resDigits[i] = resDigits[i] - 1;
		i = i - 1;
		while ((0 - 1) < i) {
			resDigits[i] = 9;
			i = i - 1;
		}

		res = new BigInt().setDigits(resDigits);
		res = res.deleteZeros();
		return res;
	}

	def multiplyForTenPow(pow: Int) : BigInt = {
		var res: BigInt;
		var resDigits: Int[];
		var i: Int;

		resDigits = new Int[pow + 1];
		i = 0;
		while (i < pow + 1) {
			resDigits[i] = 0;
			i = i + 1;
		}
		resDigits[pow] = digits[0];

		res = new BigInt().setDigits(resDigits);
		return res;
	}

	def deleteZeros() : BigInt = {
		var numOfLeadingZeros: Int;
		var i: Int;
		var newDimension: Int;
		var newDigits: Int[];
		var res: BigInt;

		numOfLeadingZeros = 0;
		i = digits.length - 1;
		while (((0 - 1) < i) && (digits[i] == 0)) {
			numOfLeadingZeros = numOfLeadingZeros + 1;
			i = i - 1;
		}

		newDimension = digits.length - numOfLeadingZeros;
		newDigits = new Int[newDimension];
		i = 0;
		while (i < newDimension) {
			newDigits[i] = digits[i];
			i = i + 1;
		}

		res = new BigInt().setDigits(newDigits);
		return res;
	}

	def setDigits(digs: Int[]) : BigInt = {
		digits = digs;
		return this;
	}

	def getLength() : Int = {
		return digits.length;
	}

	def getDigits() : Int[] = {
		return digits;
	}

	def toString(): String = {
		var str: String;
		var i: Int;

		str = "";
		i = digits.length - 1;
		while (0 < i + 1) {
			str = str + digits[i];
			i = i - 1;
		}

		return str;
	}
}