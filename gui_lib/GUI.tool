class GUI {
	native def beginGUI(name: String, width: Int, height: Int): Int
	native def clear(): Int
	native def getWidth(): Int
	native def getHeight(): Int
	native def getTime(): Int
	native def getKey(id: Int): Bool
	native def getMouseX(): Int
	native def getMouseY(): Int
	native def getMouseButton(id: Int): Bool
	native def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Int
	native def drawString(str: String, x: Int, y: Int, fontSize: Int): Int
	
	def getMousePosition(): Point = {
		return new Point().init(this.getMouseX(), this.getMouseY());
	}
}

class Point {
	var myX: Int;
	var myY: Int;

	def init(x: Int, y: Int): Point = {
		myX = x;
		myY = y;
		return this;
	}

	def x(): Int = {
		return myX;
	}

	def y(): Int = {
		return myY;
	}
	
	def plus(other: Point): Point = {
		return new Point().init(this.x() + other.x(), this.y() + other.y());
	}
	
	def times(n: Int): Point = {
		return new Point().init(myX * n, myY * n);
	}
	
	def setMagnitude(n: Int): Point = {
		var magnitude: Frac;
		var coeff: Frac;
		var math: Math;
		
		math = new Math();
		magnitude = math.sqrt(new Frac().init(myX * myX + myY * myY, 1));
		coeff = math.sqrt(new Frac().init(n, 1).divided(magnitude));
		
		return new Point().init(coeff.times(new Frac().init(myX, 1)).getRoundedInt(), coeff.times(new Frac().init(myY, 1)).getRoundedInt());
	}
		
	def draw(g: GUI): Int = {
		var useless: Int;
	
		useless = g.drawLine(myX, myY, myX, myY);

		return 0;
	}
	
	def drawLine(g: GUI, other: Point): Int = {
		var useless: Int;
	
		useless = g.drawLine(myX, myY, other.x(), other.y());

		return 0;
	}
	
	def move(x: Int, y: Int): Point = {
		return this.plus(new Point().init(x, y));
	}
	
	def isInside(r: Rect): Bool = {
		return r.topLeft().x() < myX && myX < r.topRight().x() && r.topRight().y() < myY && myY < r.bottomRight().y();
	}
	
	def toString(): String = {
		return "(" + myX + ", " + myY + ")";
	}
}

class Rect {
	var topLeft: Point;
	var width: Int;
	var height: Int;

	def init(tl: Point, w: Int, h: Int): Rect = {
		topLeft = tl;
		width = w;
		height = h;
		return this;
	}
	
	def getWidth(): Int = {
		return width;
	}
	
	def getHeight(): Int = {
		return height;
	}

	def topLeft(): Point = {
		return topLeft;
	}

	def topRight(): Point = {
		var p: Point;
		p = new Point();
		p = p.init(topLeft.x() + width, topLeft.y());
		return p;
	}

	def bottomLeft(): Point = {
		var p: Point;
		p = new Point();
		p = p.init(topLeft.x(), topLeft.y() + height);
		return p;
	}

	def bottomRight(): Point = {
		var p: Point;
		p = new Point();
		p = p.init(topLeft.x() + width, topLeft.y() + height);
		return p;
	}

	def draw(g: GUI): Int = {
		var useless: Int;
	
		useless = g.drawLine(topLeft.x(), topLeft.y(), this.topRight().x(), this.topRight().y());
		useless = g.drawLine(topLeft.x(), topLeft.y(), this.bottomLeft().x(), this.bottomLeft().y());
		useless = g.drawLine(this.bottomLeft().x(), this.bottomLeft().y(), this.bottomRight().x(), this.bottomRight().y());
		useless = g.drawLine(this.bottomRight().x(), this.bottomRight().y(), this.topRight().x(), this.topRight().y());

		return 0;
	}
	
	def move(x: Int, y: Int): Rect = {
		topLeft = topLeft.move(x, y);
		return this;
	}

	def setCenter(c: Point): Rect = {
		topLeft = new Point().init(c.x() - width/2, c.y() - height/2);
		return this;
	}
}

class Circle {
	var radius: Int;
	var center: Point;
	
	def init(c: Point, r: Int): Circle = {
		center = c;
		radius = r;
		return this;
	}
	
	def draw(g: GUI, precision: Int): Int = {
		var angle: Int;
		var start: Point;
		var p1: Point;
		var p2: Point;
		var useless: Int;
		var math: Math;
		
		angle = 360 / precision;
		math = new Math();
		start = new Point().init(0, radius);
		p1 = new Point().init(0, radius);
		
		while(1 < precision){
			p2 = math.rotate(p1, angle);
			p2 = p2.setMagnitude(radius);
			useless = p1.plus(center).drawLine(g, p2.plus(center));
			p1 = new Point().init(p2.x(), p2.y());
			precision = precision - 1;
		}
		
		useless = p1.plus(center).drawLine(g, start.plus(center));
		useless = center.draw(g);
		
		return 0;
	}
}

class Math {
	def pow(b: Int, e: Int): Int = {
		var result: Int;
		
		if(e == 0)
			result = 1;
		else if (e / 2 * 2 == e) //e is even
			result = this.pow(b * b, e / 2);
		else
			result = b * this.pow(b, e - 1);
			
		return result;
	}
	
	def abs(x: Int): Int = {
		if(x < 0)
			x = 0 - x;
		return x;
	}
	
	def sqrt(x: Frac): Frac = {
		var guess: Frac;
		var tries: Int;
		
		guess = x;
		tries = 100;
		
		while(0 < tries){
			guess = guess.plus(x.divided(guess)).divided(new Frac().init(2, 1));
			tries = tries - 1;
		}
		
		return guess;
	}

	def sin(x: Int): Frac = {
		var result: Frac;
		var x_: Frac;
		
		result = new Frac().init(1, 1);
	
		while(360 < x)
			x = x - 360;
			
		if (90 < x && x < 180 || x == 180)
			x = 180 - x;
		else if (180 < x && x < 270 || x == 270){
			x = x - 180;
			result = result.setPos(false);
		}
		else if (270 < x && x < 360 || x == 360){
			x = 360 - x;
			result = result.setPos(false);
		}
		
		if(x < 45 || x == 45){
			x_ = new Frac().init(x, 1).times(new Frac().init(174, 10000)); //174/10000 is roughly pi/180
			result = result.times(x_.minus(x_.pow(3).divided(new Frac().init(6, 1))).plus(x_.pow(5).divided(new Frac().init(120, 1))));
		}
		else
			result = result.times(this.cos(90 - x));
			
		if(result.getDenominator() == 0)
			result = new Frac().init(0, 1);
			
		return result.simplify();
	}
	
	def cos(x: Int): Frac = {
		var result: Frac;
		var x_: Frac;
		
		result = new Frac().init(1, 1);
	
		while(360 < x)
			x = x - 360;
			
		if (90 < x && x < 180 || x == 180){
			x = 180 - x;
			result = result.setPos(false);
		}
		else if (180 < x && x < 270 || x == 270){
			x = x - 180;
			result = result.setPos(false);
		}
		else if (270 < x && x < 360 || x == 360)
			x = 360 - x;
		
		if(x < 45 || x == 45){
			x_ = new Frac().init(x, 1).times(new Frac().init(174, 10000)); //174/10000 is roughly pi/180
			result = result.times(new Frac().init(1, 1).minus(x_.pow(2).divided(new Frac().init(2, 1))).plus(x_.pow(4).divided(new Frac().init(24, 1))).minus(x_.pow(6).divided(new Frac().init(720, 1))));
		}
		else
			result = result.times(this.sin(90 - x));
			
		if(result.getDenominator() == 0)
			result = new Frac().init(0, 1);
			
		return result.simplify();
	}
	
	def rotate(p: Point, angle: Int): Point = {
		var x: Int;
		var y: Int;
		
		x = new Frac().init(p.x(), 1).times(this.cos(angle)).minus(new Frac().init(p.y(), 1).times(this.sin(angle))).getRoundedInt();
		y = new Frac().init(p.x(), 1).times(this.sin(angle)).plus(new Frac().init(p.y(), 1).times(this.cos(angle))).getRoundedInt();
		
		return new Point().init(x, y);
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
		var result: Int;
		
		if(m == 0 || n == 0)
			result = 0;
		else
			result = (n*m) / this.gcd(n,m);
		
        return result;
    }
	
	def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}

class Frac {
    var numerator : Int;
    var denominator : Int;
    var sign : Bool; // true means positive.
    var math : Math;

    def init(n : Int, d : Int) : Frac = {
        math = new Math();

        numerator = math.abs(n);
        denominator = math.abs(d);
        sign = (n < 0 && d < 0 || (0 < n || n == 0) && (0 < d || d == 0)); 
        return this.simplify();
    }

    def getNumerator() : Int = {
        return numerator;
    }

    def getDenominator() : Int = {
        return denominator;
    }
	
	def getRoundedInt(): Int = {
		var result: Int;
		
		if(numerator == 0)
			result = 0;
		else if(sign){
			if(math.mod(numerator, denominator) < denominator / 2)
				result = numerator / denominator;
			else
				result = numerator / denominator + 1;
		}
		else {
			if(math.mod(numerator, denominator) < denominator / 2)
				result = 0 - (numerator / denominator + 1);
			else
				result = 0 - numerator / denominator;
		}
		
		return result;
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
				gcd_ = math.gcd(numerator, denominator);

				if(!(gcd_ == 1)) {
					numerator = numerator / gcd_;
					denominator = denominator / gcd_;
				}
			}
			
		if(numerator == 0){
			denominator = 1;
			sign = true;
		}

		while((10000 < numerator || 10000 < denominator) && !(denominator == 1 || denominator == 2)){
			if(10000 < numerator || 10000 < denominator){
				if(math.mod(numerator, 3) < math.mod(numerator, 2) && math.mod(denominator, 3) < math.mod(denominator, 2)){
					numerator = numerator / 3;
					denominator = denominator / 3;
				}
				else {
					numerator = numerator / 2;
					denominator = denominator / 2;
				}
			}
		
			if(!(numerator == 0) && !(denominator == 0)) {
				gcd_ = math.gcd(numerator, denominator);

				if(!(gcd_ == 1)) {
					numerator = numerator / gcd_;
					denominator = denominator / gcd_;
				}
			}
			
			if(numerator == 0){
				denominator = 1;
				sign = true;
			}
		}

        return this;
    }

    def plus(other : Frac) : Frac = {
        var lcm : Int;
        var lfac : Int;
        var rfac : Int;

        lcm = math.lcm(denominator, other.getDenominator());
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
	
	def pow(e: Int): Frac = {
		var result: Frac;
	
		if(e == 0)
			result = new Frac().init(1, 1);
		else if(math.mod(e, 2) == 0)
			result = this.times(this).pow(e / 2);
		else
			result = this.times(this.pow(e - 1));
			
		return result;
	}

    def inverse() : Frac = {
        return (new Frac()).init(denominator, numerator);
    }

    def negative() : Frac = {
        return (new Frac()).init(numerator, denominator).setPos(!sign);
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