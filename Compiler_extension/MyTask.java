import java.util.*;

public class MyTask extends TimerTask {
	@Override
	public void run() {
		System.out.println("Lol");
	}

	public static void main(String args[]) {
		MyTask t = new MyTask();
		Timer timer = new Timer(true);
		timer.schedule(t, 1000); // milliseconds.
		try {
	        Thread.sleep(20000);
	    } catch (InterruptedException e) {
	        e.printStackTrace();
	    }
	}
}