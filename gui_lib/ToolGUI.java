import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;

public class ToolGUI {
		private Frame mainFrame;
		private boolean[] keys = new boolean[256];
		private boolean[] mouseButtons = new boolean[2];
		private int time = 0;
		
		public int beginGUI(String name, int width, int height){
			mainFrame = new Frame();

			mainFrame.setTitle(name);
			mainFrame.setSize(width, height);
			mainFrame.setLocationRelativeTo(null);
			
			mainFrame.addWindowListener(new WindowAdapter() {
			 public void windowClosing(WindowEvent we) {
			   mainFrame.dispose();
			   System.exit(0);
			 }
			});
			
			mainFrame.addMouseListener(new MouseAdapter() {
			  public void mousePressed(MouseEvent me) {
				if (me.getButton() == 1) // left button
					mouseButtons[0] = true;
				else if (me.getButton() == 3) // right button
					mouseButtons[1] = true;
			  }
			  public void mouseReleased(MouseEvent me) {
					if (me.getButton() == 1) // left button
						mouseButtons[0] = false;
					else if (me.getButton() == 3) // right button
						mouseButtons[1] = false;
				  }
			});
			  
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
			
			for(int i = 0; i < 256; i++)
				  keys[i] = false;
			
			for(int i = 0; i < 2; i++)
				  mouseButtons[i] = false;
			
			mainFrame.setVisible(true);
			return 0;
		}
		
		public int getWidth(){
			return mainFrame.getWidth();
		}
		
		public int getHeight(){
			return mainFrame.getHeight();
		}
		
		public int getTime(){
			return time;
		}
		
		public boolean getKey(int keyCode){
			return keys[keyCode];
		}
		
		public int getMouseX(){
			java.awt.Point res = mainFrame.getMousePosition();
			if (res == null)
				return 0;
			return res.x;
		}
		
		public int getMouseY(){
			java.awt.Point res = mainFrame.getMousePosition();
			if (res == null)
				return 0;
			return res.y;
		}
		
		public boolean getMouseButton(int n){
			return mouseButtons[n];
		}
		 
		public int clear(){
			mainFrame.repaint();
			return 0;
		}
		
		public int drawLine(int x1, int y1, int x2, int y2){
			Graphics g = mainFrame.getGraphics();
			g.drawLine(x1, y1, x2, y2);
			return 0;
		}
		
		public int drawString(String str, int x, int y, int fontSize){
			Graphics g = mainFrame.getGraphics();
			Font currentFont = g.getFont();
			Font newFont = currentFont.deriveFont((float)fontSize);
			g.setFont(newFont);
			FontMetrics fontMetrics = g.getFontMetrics(newFont);
			g.drawString(str, x - fontMetrics.stringWidth(str)/2, y);
			g.setFont(currentFont);
			return 0;
		}
	}