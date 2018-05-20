package project;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;

import javax.swing.JFrame;

public class C2 extends JFrame {
	
	GeneralPath p = new GeneralPath();
	C2() {

		p.moveTo(10, 10);
		p.curveTo(10, 10, 20, 20, 10, 30);
		p.closePath();
	}

	@Override
	public void paint(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;

        g2.setStroke(new BasicStroke(2.0f));
        g2.setPaint(Color.GREEN);
        
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int[] xPoints = {10, 50, 100, 150, 200, 250, 300, 350};
        int[] yPoints = {10, 50,  10,  50,  10,  50,  10,  50};
        GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD,
            xPoints.length);

        for (int i = 0; i < yPoints.length; i++) {
			yPoints[i] += 50;
		}
        g2.draw(new Ellipse2D.Double(1.0, 2.0, 30.0, 30.0));
        // Adds point to the path by moving to the specified
        // coordinates.
        path.moveTo(xPoints[0], yPoints[0]);
        for (int i = 1; i < xPoints.length - 2; i += 4) {
            // Adds a point to the path by drawing a straight
            // line from the current position to the specified
            // coordinates.
//            path.lineTo(xPoints[i], yPoints[i]);
        	path.curveTo(xPoints[i], yPoints[i], xPoints[i+1], yPoints[i+1], xPoints[i+2], yPoints[i+2]);
        }
        path.curveTo(150, 150, 300, 300, 50, 250);
//        path.closePath();
        g2.draw(path);
        
        g2.setPaint(Color.CYAN);
        for (int i = 0; i < yPoints.length; i++) {
			g2.fillOval(xPoints[i] - 5, yPoints[i] - 5, 10, 10);
		}

        // Draw another path, a start
        g2.setPaint(Color.RED);
        g2.setStroke(new BasicStroke(2.0f));
        path = new GeneralPath(GeneralPath.WIND_NON_ZERO);
        path.moveTo(200, 50);
        path.lineTo(270, 300);
        path.lineTo(100, 120);
        path.lineTo(300, 120);
        path.lineTo(130, 300);
        path.closePath();
        g2.draw(path);

	}

	public static void main(String[] args) {
		JFrame frame = new C2();
		frame.setSize(new Dimension(800, 600));
		frame.setVisible(true);
	}

}
