object example2 {
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
		var rect: Rect;
		
		g = new GUI();
		useless = g.beginGUI("Simple frame", 800, 600);

		rect = new Rect().init(new Point().init(10, 10), 20, 100);

		time = 0;
		frameDuration = 1;
		
		while(true){
			useless = rect.draw(g);
			if(frameDuration < g.getTime() - time){
				useless = g.clear();
				useless = rect.move(1, 1).draw(g);
				time = time + frameDuration;
			}
		}
		
		return "App started";
	}
}