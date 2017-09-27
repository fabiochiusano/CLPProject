object BirthdayParadox {
	def main() : Unit = {
		println(new Computer().init(40,365).resultString());
	}
}

class Computer {
	var numOfStudents: Int;
	var daysInAYear: Int;
	var oppositeProbability: Real;

	def init(tempStudents: Int, tempDays: Int) : Computer = {
		var i: Int;
		var tempFrac: Frac;
		var tempReal: Real;

		daysInAYear = tempDays;
		numOfStudents = tempStudents;

		oppositeProbability = new Real().init(1,3);
		
		i = 0;
		while (i < numOfStudents) {
			tempFrac = new Frac().init(daysInAYear - i, daysInAYear);
			tempReal = new Real().init(0,3).evalFrac(tempFrac);
			println("tempReal = " + tempReal.toString());
			oppositeProbability = oppositeProbability.multiplyBy(tempReal);
			println("oppositeProbability = " + oppositeProbability.toString());
			println("------------------------");
			i = i + 1;
		}

		return this;
	}

	def resultString() : String = {
		return "In a class of " + numOfStudents + " students there is a probability of " + oppositeProbability.toString() + " that two of them don't have the same birthday, considering a " + daysInAYear + " days long year.";
	}
}

class Frac {
    var numerator : Int;
    var denominator : Int;
    var sign : Bool; // true means positive.
    var util : Util;

    def init(n : Int, d : Int) : Frac = {
        util = new Util();

        numerator = util.abs(n);
        denominator = util.abs(d);
        sign = (n < 0 && d < 0 || (0 < n || n == 0) && (0 < d || d == 0)); 
        return this.simplify();
    }

    def getNumerator() : Int = {
        return numerator;
    }

    def getDenominator() : Int = {
        return denominator;
    }

    def setPos(positive : Bool) : Frac = {
        sign = positive;
        return this;
    }

    def isPos() : Bool = {
        return sign;
    }

    def simplify() : Frac = {
        var gcd_ : Int;

        if(!(numerator == 0) && !(denominator == 0)) {
            gcd_ = util.gcd(numerator, denominator);

            if(!(gcd_ == 1)) {
                numerator = numerator / gcd_;
                denominator = denominator / gcd_;
            }
        }

        return this;
    }

    def plus(other : Frac) : Frac = {
        var lcm : Int;
        var lfac : Int;
        var rfac : Int;

        lcm = util.lcm(denominator, other.getDenominator());
        lfac = lcm / denominator;

        if(!sign) {
            lfac = 0 - lfac;
        }

        rfac = lcm / other.getDenominator();

        if(!other.isPos()) {
            rfac = 0 - rfac;
        }

        return (new Frac()).init((lfac * numerator) + (rfac * other.getNumerator()), lcm);
    }

    def minus(other : Frac) : Frac = {
        return this.plus(other.negative());
    }

    def times(other : Frac) : Frac = {
        return (new Frac()).init(numerator * other.getNumerator(), denominator * other.getDenominator()).simplify().setPos(this.isPos() && other.isPos() || !this.isPos() && !other.isPos());
    }

    def divided(other : Frac) : Frac = {
        return this.times(other.inverse());
    }

    def inverse() : Frac = {
        return (new Frac()).init(denominator, numerator);
    }

    def negative() : Frac = {
        return (new Frac()).init(numerator, denominator).setPos(false);
    }

    def toString() : String = {
        var result : String;
        if(sign) {
            result = "";
        } else {
            result = "-";
        }
        return result + numerator + "/" + denominator;
    }
}

class Real {
    var integerPart : Int;
    var digits : Int[];
    var util : Util;

    def init(intPart : Int, digitsCount: Int): Real = {
        var i : Int;

        util = new Util();
        integerPart = intPart;
        digits = new Int[digitsCount];
        i = 0;
        while(i < digitsCount) {
            digits[i] = 0;
            i = i + 1;
        }
        return this;
    }

    def getDigits() : Int[] = {
        return digits;
    }

    def setDigits(d : Int[]) : Real = {
    	digits = d;
    	return this;
    }

    def getIntegerPart() : Int = {
        return integerPart;
    }

    def setIntegerPart(p : Int) : Real = {
        integerPart = p;
        return this;
    }

    def evalFrac(frac : Frac) : Real = {
        var leftover : Int;
        var i : Int;
        var den : Int;

        den = frac.getDenominator();
        integerPart = frac.getNumerator() / den;
        if(!frac.isPos()) {
            integerPart = 0 - integerPart;
        }
        leftover = util.mod(frac.getNumerator(), den);

        i = 0;
        while(i < digits.length) {
            leftover = 10 * leftover;
            digits[i] = leftover / den;
            leftover = util.mod(leftover, den);
            i = i + 1;
        }
        return this;
    }

    // note that this only works for positive reals
    def plus(other : Real) : Real = {
        var len : Int;
        var od : Int[];
        var resDig : Int[];
        var carry : Int;
        var i : Int;
        var sum : Int;
        var result : Real;

        od = other.getDigits();
        // taking the max length ensures that it will crash if they don't match :P
        if(digits.length < od.length) {
            len = od.length;
        } else {
            len = digits.length;
        }

        result = new Real().init(0, len);
        resDig = result.getDigits();

        carry = 0;
        i = len - 1;

        while(!(i < 0)) {
            sum = digits[i] + od[i] + carry;
            carry = sum / 10;
            //println(digits[i] + " + " + od[i] + " = " + sum + "(" + carry + ")");
            resDig[i] = util.mod(sum, 10);
            i = i - 1;
        } 

        return result.setIntegerPart(integerPart + other.getIntegerPart() + carry);
    }

    def multiplyBy(other: Real): Real = {
    	var tempResults: Int[];
    	var resultInt: Int;
    	var resultDigits: Int[];
    	var correctResult: Real;

    	var tempResult: Int;
    	var multResult: Int;
    	var carry: Int;
    	var singleDigitPart: Int;
    	var i: Int;
    	var j: Int;
    	var mult: Int;
    	var lengthD: Int;
    	var secondTerm: Int;

    	var tempNum: Int;
    	var reminder: Int;

    	lengthD = this.getDigits().length;
    	tempResults = new Int[lengthD + 1];
    	i = 0;
    	j = 0;
    	carry = 0;
    	mult = 1;
    	while (i < lengthD + 1) {
    		if (i < lengthD) {
    			secondTerm = other.getDigits()[lengthD - i - 1];
    		}
    		else {
    			secondTerm = other.getIntegerPart();
    		}
    		while (j < lengthD) {
    			multResult = this.getDigits()[lengthD - j - 1] * secondTerm + carry;
    			carry = multResult / 10;
    			singleDigitPart = multResult - (carry * 10);
    			tempResults[i] = tempResults[i] + (singleDigitPart * mult);
    			mult = mult * 10;
    			j = j + 1;
    		}
    		multResult = this.getIntegerPart() * secondTerm + carry;
    		carry = multResult / 10;
    		singleDigitPart = multResult - (carry * 10);
    		tempResults[i] = tempResults[i] + (singleDigitPart * mult);
    		println("tempResults[" + i + "] = " + tempResults[i]);
    		
			mult = new Util().pow(10, i + 1);
    		j = 0;
    		i = i + 1;
    	}

    	i = 0;
    	resultInt = 0;
    	while (i < tempResults.length) {
    		resultInt = resultInt + tempResults[i];
    		i = i + 1;
    	}
    	println("resultInt = " + resultInt);

    	i = 0;
    	if (other.getIntegerPart() == 1) {
    		correctResult = new Real().init(1,lengthD);
    	}
    	else {
    		resultDigits = new Int[lengthD];
    		while (i < lengthD * 2) {
    			tempNum = resultInt / 10;
    			reminder = resultInt - (tempNum * 10);
    			resultInt = tempNum;
    			if (!(i < lengthD)) {
    				resultDigits[lengthD * 2 - 1 - i] = reminder;
    			}
    			i = i + 1;
    		}

    		correctResult = new Real().init(0, 3);
    		correctResult = correctResult.setDigits(resultDigits);
    	}

    	return correctResult;
    }

    def toString() : String = {
        var ret : String;
        var i : Int;

        ret = "" + integerPart + ".";
        i = 0;
        while(i < digits.length) {
            ret = ret + digits[i];
            i = i + 1;
        }
        return ret;
    }
}

class Util {
	def pow(base: Int, exp: Int): Int = {
		var temp: Int;
		if (exp == 0) {
			temp = 1;
		}
		else {
			temp = base * this.pow(base, exp - 1);
		}
		return temp;
	}

    def abs(v : Int) : Int = {
        var res : Int;

        if(!(v < 0)) {
            res = v;
        } else {
            res = 0 - v;
        }
        return res;
    }

    def gcd(m_ : Int, n_ : Int) : Int = {
        var t : Int;
        var r : Int;
        var result : Int;
        var m : Int;
        var n : Int;

        m = this.abs(m_);
        n = this.abs(n_);

        if (m < n) {
            t = m;
            m = n;
            n = t;
        }

        r = this.mod(m,n); // m % n;

        if (r == 0) {
            result = n;
        } else {
            result = this.gcd(n, r);
        }
        return result;
    }

    def lcm(m : Int, n : Int) : Int = {
        return (n*m) / this.gcd(n,m);
    }

    def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}