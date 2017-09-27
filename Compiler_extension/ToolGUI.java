import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;

public class ToolGUI {
		private Frame mainFrame;
		private boolean[] keys = new boolean[256];
		private int time = 0;
		
		public void beginGUI(){
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
		
		public void clear(){
			mainFrame.repaint();
		}
		
		public void drawLine(int x1, int y1, int x2, int y2){
			Graphics g = mainFrame.getGraphics();
			g.drawLine(x1, y1, x2, y2);
			return;
		}
	}