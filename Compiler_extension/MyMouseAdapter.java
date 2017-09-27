import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;

public class MyMouseAdapter extends MouseAdapter {
  public void mousePressed(MouseEvent me) {
    System.out.println(me.getButton());
  }
}