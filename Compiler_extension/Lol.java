import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;

public class Lol {    

  public static void main(String[] args) {

    Frame f = new Frame();

    f.setTitle("Sample app");
    f.setSize(400, 400);
    f.setLocationRelativeTo(null);
    f.addWindowListener(new WindowAdapter() {
     public void windowClosing(WindowEvent we) {
       f.dispose();
       System.exit(0);
     }
    });
    f.addMouseListener(new MouseAdapter() {
      public void mousePressed(MouseEvent me) {
        System.out.println(me.getButton());
      }
    });
    boolean[] keys = new boolean[256];
    for(int i = 0; i < 256; i++)
      keys[i] = false;
    f.addKeyListener(new KeyAdapter() {
      public void keyPressed(KeyEvent ke) {
        int c = ke.getKeyChar();
        keys[c] = true;
      }
      public void keyReleased(KeyEvent ke) {
        int c = ke.getKeyChar();
        keys[c] = false;
      }
    });
    f.setVisible(true);



    paintCircle(f, 50, new Point(200, 200));
    //paintCircle(f, 100, new Point(150, 150));

    while(true) 
      for(int i = 0; i < 256; i++)
        if(keys[i])
          System.out.println((char)i + " pressed");
  }

  private static void paintCircle(Frame f, int radius, Point center) {
    int x, y;

    Graphics g1 = f.getGraphics();

    g1.drawLine(0, 0, 400, 400);
    /*
    for(x = center.x, y = center.y - radius; y <= center.y; x++, y++)
     g1.drawLine(x, y, x, y);

    for(x = center.x + radius, y = center.y; x >= center.x; x--, y++)
     g1.drawLine(x, y, x, y);

    for(x = center.x, y = center.y + radius; y >= center.y; x--, y--)
     g1.drawLine(x, y, x, y);

    for(x = center.x - radius, y = center.y; x <= center.x; x++, y--)
     g1.drawLine(x, y, x, y);
    */
  }
}