import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;

public class Pong {    

  public static void main(String[] args) {

    ToolGUI.beginGUI();  

	try {
    	Thread.sleep(100);      
	} catch(InterruptedException ex) {
    	Thread.currentThread().interrupt();
	}

    paintCircle(50, new Point(200, 200));
    paintCircle(100, new Point(150, 150));
	
	BouncingCube cube = new BouncingCube();
	cube.init(new TPoint(30, 10), 50, new TPoint(1, 2));
	UserCube player = new UserRect();
	player.init(new TPoint(100, 100), 30, 'w', 's', 'a', 'd');
	
	int time = 0;
	while(true){
		System.out.print("");
		if(time != ToolGUI.getTime()){
			ToolGUI.clear();
			System.out.println("Seconds elapsed: " + (time / 100.0f));
			cube.doDrawing();
			player.doDrawing();
			time = ToolGUI.getTime();
		}
	}
  }

  private static void paintCircle(int radius, Point center) {
    int x, y;

    for(x = center.x, y = center.y - radius; y <= center.y; x++, y++)
     ToolGUI.drawLine(x, y, x, y);

    for(x = center.x + radius, y = center.y; x >= center.x; x--, y++)
     ToolGUI.drawLine(x, y, x, y);

    for(x = center.x, y = center.y + radius; y >= center.y; x--, y--)
     ToolGUI.drawLine(x, y, x, y);

    for(x = center.x - radius, y = center.y; x <= center.x; x++, y--)
     ToolGUI.drawLine(x, y, x, y);
  }

	static class ToolGUI {
		private static Frame mainFrame;
		private static boolean[] keys = new boolean[256];
		private static int time = 0;
		
		public static void beginGUI(){
			mainFrame = new Frame();

			mainFrame.setTitle("Test");
			mainFrame.setSize(400, 400);
			mainFrame.setLocationRelativeTo(null);
			
			mainFrame.addWindowListener(new WindowAdapter() {
			 public void windowClosing(WindowEvent we) {
			   mainFrame.dispose();
			   System.exit(0);
			 }
			});
			
			mainFrame.addMouseListener(new MouseAdapter() {
			  public void mousePressed(MouseEvent me) {
				System.out.println(me.getButton());
			  }
			});
			
			for(int i = 0; i < 256; i++)
			  keys[i] = false;
			  
			mainFrame.addKeyListener(new KeyAdapter() {
			  public void keyPressed(KeyEvent ke) {
				int c = ke.getKeyChar();
				keys[c] = true;
			  }
			  public void keyReleased(KeyEvent ke) {
				int c = ke.getKeyChar();
				keys[c] = false;
			  }
			});
			
			new Timer(10, new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					time++;
				}
			}).start();
			
			mainFrame.setVisible(true);
			return;
		}
		
		public static int getWidth(){
			return mainFrame.getWidth();
		}
		
		public static int getHeight(){
			return mainFrame.getHeight();
		}
		
		public static int getTime(){
			return time;
		}
		
		public static boolean getKey(int keyCode){
			return keys[keyCode];
		}
		
		public static void clear(){
			mainFrame.repaint();
		}
		
		public static void drawLine(int x1, int y1, int x2, int y2){
			Graphics g = mainFrame.getGraphics();
			g.drawLine(x1, y1, x2, y2);
			return;
		}
	}
}

	class TPoint {
		public float x;
		public float y;
		
		public TPoint(float x, float y){
			this.x = x;
			this.y = y;
		}
		
		public void move(TPoint p){
			this.x += p.x;
			this.y += p.y;
		}
		
		public int xInt(){
			return Math.round(this.x);
		}
		
		public int yInt(){
			return Math.round(this.y);
		}
	}

	class BouncingCube {  
		private TPoint a, b, c, d;
		private TPoint dir;
	  
		public void init(TPoint topLeft, int size, TPoint dir){
			this.a = topLeft;
			this.b = new TPoint(topLeft.x + size, topLeft.y);
			this.c = new TPoint(topLeft.x + size, topLeft.y + size);
			this.d = new TPoint(topLeft.x, topLeft.y + size);
			this.dir = dir;
			
			doDrawing();
		}

		public void doDrawing() {			
			Pong.ToolGUI.drawLine(a.xInt(), a.yInt(), b.xInt(), b.yInt());
			Pong.ToolGUI.drawLine(b.xInt(), b.yInt(), c.xInt(), c.yInt());
			Pong.ToolGUI.drawLine(c.xInt(), c.yInt(), d.xInt(), d.yInt());
			Pong.ToolGUI.drawLine(d.xInt(), d.yInt(), a.xInt(), a.yInt());
			
			updatePoints(dir);
		}
	  
		private void updatePoints(TPoint dir){
			this.a.move(dir);
			this.b.move(dir);
			this.c.move(dir);
			this.d.move(dir);
		  
			if(d.y > Pong.ToolGUI.getHeight())
				dir.y = - Math.abs(dir.y);
			else if(a.y < 0)
				dir.y = Math.abs(dir.y);
			if(c.x > Pong.ToolGUI.getWidth())
				dir.x = - Math.abs(dir.x);
			else if(a.x < 0)
				dir.x = Math.abs(dir.x);
		}
	}
	
	class UserRect {  
		private TPoint a, b, c, d;
		private TPoint dir;
		private int up, down, left, right;
	  
		public void init(TPoint topLeft, int size, int up, int down, int left, int right){
			this.a = topLeft;
			this.b = new TPoint(topLeft.x + size, topLeft.y);
			this.c = new TPoint(topLeft.x + size, topLeft.y + size);
			this.d = new TPoint(topLeft.x, topLeft.y + size);
			this.up = up;
			this.down = down;
			this.left = left;
			this.right = right;
			
			doDrawing();
		}
		
		public void init(TPoint topLeft, int sizeX, int sizeY, int up, int down, int left, int right){
			this.a = topLeft;
			this.b = new TPoint(topLeft.x + sizeX, topLeft.y);
			this.c = new TPoint(topLeft.x + sizeX, topLeft.y + sizeY);
			this.d = new TPoint(topLeft.x, topLeft.y + sizeY);
			this.up = up;
			this.down = down;
			this.left = left;
			this.right = right;
			
			doDrawing();
		}

		public void doDrawing() {			
			Pong.ToolGUI.drawLine(a.xInt(), a.yInt(), b.xInt(), b.yInt());
			Pong.ToolGUI.drawLine(b.xInt(), b.yInt(), c.xInt(), c.yInt());
			Pong.ToolGUI.drawLine(c.xInt(), c.yInt(), d.xInt(), d.yInt());
			Pong.ToolGUI.drawLine(d.xInt(), d.yInt(), a.xInt(), a.yInt());
			
			updatePoints();
		}
	  
		private void updatePoints(){
			int x = 0, y = 0;
			
			if(Pong.ToolGUI.getKey(up))
				y = -1;
			else if(Pong.ToolGUI.getKey(down))
				y = 1;
			if(Pong.ToolGUI.getKey(left))
				x = -1;
			else if(Pong.ToolGUI.getKey(right))
				x = 1;
				
			TPoint dir = new TPoint(x, y);
		
			if((d.y < Pong.ToolGUI.getHeight() && a.y > 0 && c.x < Pong.ToolGUI.getWidth() && a.x > 0) ||
				(d.y >= Pong.ToolGUI.getHeight() && y <= 0) ||
				(a.y <= 0 && y >= 0) ||
				(c.x >= Pong.ToolGUI.getWidth() && x <= 0) ||
				(a.x <= 0 && x >= 0)){
				this.a.move(dir);
				this.b.move(dir);
				this.c.move(dir);
				this.d.move(dir);
			}
		}
	}