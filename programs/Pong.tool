object Pong {
    def main() : Unit = {
		println(new Console().run());        
    }
}

class Console {
	def run(): String = {
		var player1: Player;
		var player2: Player;
		var ball: Ball;
		var g: GUI;
		var useless: Int;
		var time: Int;
		var frameDuration: Int;
		var frameWidth: Int;
		var frameHeight: Int;
		
		g = new GUI();
		frameWidth = 800;
		frameHeight = 600;
		useless = g.beginGUI("Pong", frameWidth, frameHeight);
		
		player1 = new Player().init(new Rect().init(new Point().init(10, 10), 20, 100), 119, 115, 5, 1);  //w up, s down
		player2 = new Player().init(new Rect().init(new Point().init(g.getWidth() - 40, 10), 20, 100), 111, 108, 5, 0-1); //o up, l down
		ball = new Ball().init(new Rect().init(new Point().init(400, 300), 15, 15), player1, player2, 1, new Point().init(1, 1));
		
		time = 0;
		frameDuration = 1;
		
		while(true){
			useless = player1.draw(g);
			useless = player2.draw(g);
			useless = ball.draw(g);
			if(frameDuration < g.getTime() - time){
				useless = g.clear();
				useless = player1.move(g).draw(g);
				useless = player2.move(g).draw(g);
				useless = ball.move().draw(g);
				time = time + frameDuration;
			}
		}
		
		return "End";
	}
}

class Player {
	var position: Rect;
	var up: Int;
	var down: Int;
	var speed: Int;
	var points: Int;
	var pointStile: Int; // 1 left to right, -1 right to left
	
	def init(pos: Rect, u: Int, d: Int, s: Int, ps: Int): Player = {
		position = pos;
		up = u;
		down = d;
		speed = s;
		points = 0;
		pointStile = ps;
		return this;
	}
	
	def getPosition(): Rect = {
		return position;
	}
	
	def draw(g: GUI): Int = {
		var useless: Int;
		var rect: Rect;
		var i: Int;
		var pt: Int;
		
		useless = position.draw(g);
		
		i = 0;
		pt = points;
		while(0 < pt){
			if(pointStile < 0)
				rect = new Rect().init(new Point().init(720 - 20 * i, 40), 10, 10);
			else
				rect = new Rect().init(new Point().init(80 + 20 * i, 40), 10, 10);				
			useless = rect.draw(g);
			i = i + 1;
			pt = pt - 1;
		}
		
		return 0;
	}
	
	def addPoints(n: Int): Int = {
		points = points + n;
		return 0;
	}
	
	def move(g: GUI): Player = {
		var dir: Int;
		
		if(g.getKey(up))
			dir = 0-1;
		else if(g.getKey(down))
			dir = 1;
		else
			dir = 0;
			
		position = position.move(0, dir * speed);
		
		return this;
	}
}

class Ball {
	var initialPosition: Rect;
	var position: Rect;
	var player1: Player;
	var player2: Player;
	var speed: Int;
	var initialSpeed: Int;
	var dir: Point;
	
	def init(pos: Rect, p1: Player, p2: Player, s: Int, d: Point): Ball = {
		position = pos;
		initialPosition = new Rect().init(pos.topLeft(), pos.getWidth(), pos.getHeight());
		player1 = p1;
		player2 = p2;
		speed = s;
		initialSpeed = s;
		dir = d;
		return this;
	}
	
	def draw(g: GUI): Int = {
		return position.draw(g);
	}
	
	def move(): Ball = {
		var useless: Int;
	
		if((position.topLeft().isInside(player1.getPosition()) || position.bottomLeft().isInside(player1.getPosition())) && dir.x() < 0){
			dir = new Point().init(dir.x() * (0-1), dir.y());
			speed = speed + 1;
		}
		else if((position.topRight().isInside(player2.getPosition()) || position.bottomRight().isInside(player2.getPosition())) && 0 < dir.x()){
			dir = new Point().init(dir.x() * (0-1), dir.y());
			speed = speed + 1;
		}
			
		if(position.topRight().y() < 25)	
			dir = new Point().init(dir.x(), dir.y() * (0-1));
		if(600 < position.bottomRight().y())	
			dir = new Point().init(dir.x(), dir.y() * (0-1));
			
		position = position.move(dir.x() * speed, dir.y() * speed);
		
		if(position.topRight().x() < 0){
			useless = player2.addPoints(1);
			position = new Rect().init(initialPosition.topLeft(), initialPosition.getWidth(), initialPosition.getHeight());
			speed = initialSpeed;
		}
		else if(800 < position.topLeft().x()){
			useless = player1.addPoints(1);
			position = new Rect().init(initialPosition.topLeft(), initialPosition.getWidth(), initialPosition.getHeight());
			speed = initialSpeed;
		}
		
		return this;
	}
}