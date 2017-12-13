package uk.ac.cam.gurdon;


/** Simple utility class to avoid javax.vecmath because iProducts can't handle it.
 */
public class Point2d {
public double x,y;
	
	public Point2d(){
		
	}

	public Point2d(double x, double y) {
		this.x = x;
		this.y = y;
	}

	public double distance(Point2d other){
		return Math.sqrt( ((this.x-other.x)*(this.x-other.x)) + ((this.y-other.y)*(this.y-other.y)) );
	}
	
}
