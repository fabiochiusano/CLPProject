import java.awt.*;        // Using AWT container and component classes
import java.awt.event.*;  // Using AWT event classes and listener interfaces
import java.awt.geom.*;

// An AWT program inherits from the top-level container java.awt.Frame
public class AWTCounter extends Frame implements ActionListener, MouseListener {
   private Label lblCount;    // Declare component Label
   private TextField tfCount; // Declare component TextField
   private Button btnCount;   // Declare component Button
   private int count = 0;     // Counter's value
 
   /** Constructor to setup GUI components and event handling */
   public AWTCounter () {
      setLayout(new FlowLayout());
         // "super" Frame sets its layout to FlowLayout, which arranges the components
         //  from left-to-right, and flow to next row from top-to-bottom.
 
      lblCount = new java.awt.Label("Counter");  // construct Label
      add(lblCount);                    // "super" Frame adds Label
 
      tfCount = new TextField("0", 10); // construct TextField
      tfCount.setEditable(false);       // set to read-only
      add(tfCount);                     // "super" Frame adds tfCount
 
      btnCount = new Button("Count");   // construct Button
      add(btnCount);                    // "super" Frame adds Button
 
      btnCount.addActionListener(this);
         // Clicking Button source fires ActionEvent
         // btnCount registers this instance as ActionEvent listener
 
      setTitle("AWT Counter");  // "super" Frame sets title
      setSize(250, 100);        // "super" Frame sets initial window size
 
      // System.out.println(this);
      // System.out.println(lblCount);
      // System.out.println(tfCount);
      // System.out.println(btnCount);
      
      addMouseListener(this);

      addWindowListener(new WindowAdapter()
       {public void windowClosing(WindowEvent e)
          {dispose(); System.exit(0);}
       }
    );

      setVisible(true);         // "super" Frame shows
 
      // System.out.println(this);
      // System.out.println(lblCount);
      // System.out.println(tfCount);
      // System.out.println(btnCount);
   }
 
   /** The entry main() method */
   public static void main(String[] args) {
      // Invoke the constructor to setup the GUI, by allocating an instance
      AWTCounter app = new AWTCounter();
      /*
      Frame app = new Frame();
      app.setLayout(new FlowLayout());
      app.setSize(250, 100);
      app.setTitle("Trololol");

      Label l = new Label();
      l.setText("Heila");
      app.add(l);

      Button b = new Button("Click me");
      b.addActionListener(app);
      app.add(b);

      app.setVisible(true);*/
   }

   public void paint(Graphics g) {
      Graphics2D g2 = (Graphics2D) g;
      g2.setColor(Color.red);
      g2.drawRect(50, 50, 200, 200);
      Point2D.Double point = new Point2D.Double(60, 60);
      g2.draw(new Line2D.Double(60, 60, 80, 60));;
   }
 
   /** ActionEvent handler - Called back upon button-click. */
   @Override
   public void actionPerformed(ActionEvent evt) {
      ++count; // increase the counter value
      // Display the counter value on the TextField tfCount
      tfCount.setText(count + ""); // convert int to String
   }

   @Override
   public void mousePressed(MouseEvent e)  { 
      System.out.println("Mouse-button pressed!"); 
   }


   @Override
   public void mouseReleased(MouseEvent e) {
      System.out.println("Mouse-button released!"); 
   }
 
   @Override
   public void mouseClicked(MouseEvent e)  {
      count = 0;
      tfCount.setText(count + "");
      System.out.println("Mouse-button clicked (pressed and released)!"); 
   }
 
   @Override
   public void mouseEntered(MouseEvent e)  { 
      System.out.println("Mouse-pointer entered the source component!"); 
   }
 
   @Override
   public void mouseExited(MouseEvent e)   {
      System.out.println("Mouse exited-pointer the source component!");  
   }
}