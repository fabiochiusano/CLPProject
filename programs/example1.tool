object example1 {
    def main() : Unit = {
		println(new Console().run());     
    }
}

class Console {
	def run(): String = {
		var g: GUI;
		var useless: Int;
		
		g = new GUI();
		useless = g.beginGUI("Simple frame", 800, 600);

		useless = g.drawString("Hello world!", g.getWidth()/2, g.getHeight()/2, 50);
		
		return "App started";
	}
}