package project;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class C1 {

	static final double C = 20e-9;  
	static final double L = 2e-3;  
	
	static double A(double omega) {
		return 1 - 1/(Math.pow(omega, 2) * L * C);
	}
	
	static double Z(double omega) {
		double z = 2 * (L * Math.pow(omega, 2) * C - 1) / (Math.pow(omega, 2) * C * C);
		return Math.sqrt(Math.abs(z));
	}
	
	static double alpha(double omega) {
		double a = A(omega);
		if (Math.abs(a) > 1) {
			double sum = Math.abs(a) + Math.sqrt(a * a - 1);
			return Math.log(sum);
		}
		else {
			return 0.0;
		}
	}	 

	static double beta(double omega) {
		double a = A(omega);
		if (a >= 1) {
			return 0.0;
		}
		else if (Math.abs(a) < 1) {
			return Math.acos(a);
		}
		else {
			return Math.PI;
		}
	}
	
	static double omega(double frequency) {
		return 2 * Math.PI * frequency;
	}

	static int nerq = 0;
	
	public static void main(String[] args) {
		double x0 = 1e5, x1 = 2e5;
		int n = 1000;
		double d = (x1 - x0) / n;
		Path2D path = new Path2D.Double();
		path.moveTo(x0, A(x0));
		List<Double> xs = new ArrayList<>(), ys = new ArrayList<>();
		
		for (int i = 0; i < n; ++i) {
			double x = x0 + d*i;
			xs.add(x);
			ys.add(A(x));
		}
		double yMax = Collections.max(ys);
		double yMin = Collections.min(ys);
		double yAbsMax = Math.max(Math.abs(yMax), Math.abs(yMin));
		
		JFrame frame = new JFrame();
		
		JPanel canvas = new JPanel() {

			@Override
			public void paint(java.awt.Graphics g) {
				Graphics2D g2 = (Graphics2D) g;
				int h = getHeight();
				int w = getWidth();
				g2.drawLine(0, h/2, w, h/2);
				g2.drawLine(w/2, 0, w/2, h);

				g2.fillOval(100, 100, 50, 50);
				Path2D p2 = new Path2D.Double();
				
				AffineTransform transform = g2.getTransform();
				transform.translate(0, h/2);
				transform.scale(1, -1);
				
				g2.setColor(Color.BLUE);
				g2.draw(transform.createTransformedShape(new Line2D.Double(0, 0, 100, 600)));
				g2.draw(transform.createTransformedShape(new Line2D.Double(0, 0, 100, 300)));
				g2.draw(transform.createTransformedShape(new Line2D.Double(0, 0, 100, 100)));
				
				
				transform.scale(w/(x1-x0), h/(yAbsMax*2));
				transform.translate(-x0, 0);
//				transform.translate(-x0, 0.0);

				g2.draw(transform.createTransformedShape(new Line2D.Double(0, 0, 10000, 0.5)));
				g2.draw(transform.createTransformedShape(new Line2D.Double(0, 0, 10000, 1.5)));
				g2.draw(transform.createTransformedShape(new Line2D.Double(0, 0, 10000, 1.)));
				
				p2.moveTo(0, 0);
				
				for (int i = 0; i < xs.size(); ++i) {
					p2.lineTo(xs.get(i), ys.get(i));
//					System.out.printf("%.4f %.4f\n", xs.get(i), ys.get(i));
				}
				
//				g2.draw(p2);
				g2.draw(transform.createTransformedShape(p2));
				g2.setColor(Color.RED);
				g2.draw(transform.createTransformedShape(new Line2D.Double(x0, 0, x1-20000, 1.)));
				g2.setColor(Color.GREEN);
				g2.draw(transform.createTransformedShape(new Line2D.Double(x1 - 2000, 0, x0, 1.)));

				g2.draw(transform.createTransformedShape(new Line2D.Double(x0, -1.5, x1-20000, 1.)));
				g2.draw(transform.createTransformedShape(new Line2D.Double(x0, 1.5, x1-20000, 1.)));
			};
		};
		frame.add(canvas);
		frame.setVisible(true);
		frame.setSize(800, 600);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	private static void e() {
		List<Double> xs = Arrays.asList(1.0, 10.0, 100.0, 1000.0, 1e4, 1e5, 2e5, 3e5, 4e5, 6e5, 1e6, 1e7, 1e8);
		xs.forEach(x -> System.out.printf("%15.4f -> %10.4f\n", x, A(x)));
		System.out.println("===================");
		xs.forEach(x -> System.out.printf("%f -> %f\n", x, Z(x)));
		System.out.println("===================");
		xs.forEach(x -> System.out.printf("%f -> %f\n", x, alpha(x)));
		System.out.println("===================");
		xs.forEach(x -> System.out.printf("%f -> %f\n", x, beta(x)));
	}

}
