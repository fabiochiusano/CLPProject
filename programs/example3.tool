object example3 {
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
		var frameWidth: Int;
		var frameHeight: Int;
		var square: Rect;
		var squareSide: Int;
		var c: Point; // movement center
		var r: Int; // movement radius
		var math: Math;
		
		g = new GUI();
		frameWidth = 800;
		frameHeight = 600;
		useless = g.beginGUI("Simple frame", frameWidth, frameHeight);

		squareSide = 20;
		c = new Point().init(frameWidth/2, frameHeight/2);
		r = frameHeight/2 - 50;
		square = new Rect().init(new Point().init(c.x() - squareSide/2 + r, c.y() - squareSide/2), squareSide, squareSide);

		time = 0;
		frameDuration = 1;
		math = new Math();

		while(true){
			useless = square.draw(g);
			if(frameDuration < g.getTime() - time){
				useless = g.clear();
				square = square.setCenter(new Point().init(
					c.x() + math.cos(g.getTime()).times(new Frac().init(r, 1)).getRoundedInt() - squareSide/2,
					c.y() + math.sin(g.getTime()).times(new Frac().init(r, 1)).getRoundedInt() - squareSide/2
				));
				time = time + frameDuration;
			}
		}
		
		return "App started";
	}
}