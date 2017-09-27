object Calculator {
    def main() : Unit = {
		println(new TestGUI().test());        
    }
}

class TestGUI {
	def test(): String = {
		var g: GUI;
		var p: Point;
		var r: Rect;
		var c1: Circle;
		var c2: Circle;
		var useless: Int;
		var time: Int;
		var circlePrecision: Int;
		
		g = new GUI();
		p = new Point().init(50, 50);
		r = new Rect().init(p, 200, 300);
		c1 = new Circle().init(new Point().init(200, 200), 100);
		c2 = new Circle().init(new Point().init(200, 200), 25);
		useless = g.beginGUI("test", 400, 400);

		time = 0;
		circlePrecision = 3;
		while(true){
			//useless = r.draw(g);
			useless = c1.draw(g, circlePrecision);
			useless = c2.draw(g, circlePrecision);
			if(50 < g.getTime() - time){
				useless = g.clear();
				circlePrecision = circlePrecision + 1;
				time = time + 50;
			}
		}
		
		return "end";
	}
}