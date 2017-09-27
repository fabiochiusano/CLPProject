object example4 {
    def main() : Unit = {
		println(new Console().run());     
    }
}

class Console {
	def run(): String = {
		var g: GUI;
		var useless: Int;
		var time: Int;
		var frameDuration: Int;
		var square: MySquare;
		
		g = new GUI();
		useless = g.beginGUI("Prova 3", 800, 600);

		square = new MySquare().init(new Point().init(10, 30), 20);

		time = 0;
		frameDuration = 1;
		
		while(true){
			useless = square.draw(g);
			if(frameDuration < g.getTime() - time){
				useless = g.clear();
				useless = square.move(g).draw(g);
				time = time + frameDuration;
			}
		}
		
		return "App started";
	}
}

class MySquare {
	var square: Rect;
	var sideLength: Int;

	def init(topLeft: Point, sL: Int): MySquare = {
		square = new Rect().init(topLeft, sL, sL);
		sideLength = sL;
		return this;
	}
	
	def move(g: GUI): MySquare = {
		var dirX: Int;
		var dirY: Int;
		
		if(g.getKey(119)) // w, go up
			dirY = 0-1;
		else if(g.getKey(115)) // s, go down
			dirY = 1;
		else 
			dirY = 0;
		if(g.getKey(97)) // a, go left
			dirX = 0-1;
		else if(g.getKey(100)) // d, go right
			dirX = 1;
		else
			dirX = 0;
			
		square = square.move(dirX, dirY);
		
		return this;
	}

	def draw(g: GUI): Int = {
		var useless: Int;
		useless = square.draw(g);
		return 0;
	}
}