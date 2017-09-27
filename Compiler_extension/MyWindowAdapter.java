import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;

public class MyWindowAdapter extends WindowAdapter {
  public void windowClosing(WindowEvent we) {
    //f.dispose();
    System.exit(0);
  }
}