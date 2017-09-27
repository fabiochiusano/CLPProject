object Sample {
    def main() : Unit = {
        println(new App().init());        
    }
}

class App {
	def init() : String = {
		var time: Int;
		var cube: Cube;
		var useless: Int;

		ToolGUI.beginGUI("Sample app", 400, 400);

		cube = new Cube();
		useless = cube.init(new Point(10, 200), 20);
		useless = cube.doDrawing();

		time = 0;
		while (true) {
			if (time < ToolGUI.getTime()) {
				ToolGUI.clear();

				useless = cube.moveRight();
				useless = cube.doDrawing();

				time = ToolGUI.getTime();
			}
		}

		return "App started.";
	}
}

class Cube {
	var side: Int;
	var center: Point;

	def init(c: Point, s: Int): Int = {
		this.center = c;
		this.side = s;
		return 1; 
	}

	def moveRight(): Int = {
		var newX = this.center.x + 1;
		var newY = this.center.y;
		this.center = new Point(newX, newY);
		return 1;
	}

	def doDrawing(): Int = {
		var blx = this.center.x - this.side/2;
		var bly = this.center.y - this.side/2;
		var brx = this.center.x + this.side/2;
		var bry = this.center.y - this.side/2;
		var tlx = this.center.x - this.side/2;
		var tly = this.center.y + this.side/2;
		var trx = this.center.x + this.side/2;
		var try = this.center.y + this.side/2;
		ToolGUI.drawLine(new Point(blx, bly), new Point(brx, bry));
		ToolGUI.drawLine(new Point(brx, bry), new Point(trx, try));
		ToolGUI.drawLine(new Point(trx, try), new Point(tlx, tly));
		ToolGUI.drawLine(new Point(tlx, tly), new Point(blx, bly));
		return 1;
	}
}