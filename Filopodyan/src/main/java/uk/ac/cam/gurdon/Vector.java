package uk.ac.cam.gurdon;
import java.awt.geom.Point2D;


/**
 * This class represents a mathematical vector as a direction and magnitude.
 * 
 * Note, this class has no relation to java.util.Vector!
 * 
 * @author Robert C. Duvall
 */
public class Vector {
    // angle in degrees
    private double myAngle;
    // "speed" in pixels per second
    private double myMagnitude;


    /**
     * Create a zero vector, i.e., with no magnitude.
     */
    public Vector () {
        this(0, 0);
    }

    /**
     * Create a vector in the given direction with the given magnitude.
     */
    public Vector (double angle, double magnitude) {
        setDirection(angle);
        setMagnitude(magnitude);
    }

    /**
     * Create a vector whose direction and magnitude are determined by
     * direction and distance between the two given points.
     */
    public Vector (Point2D source, Point2D target) {
        double dx = target.getX() - source.getX();
        double dy = source.getY() - target.getY();
        setDirection(angleBetween(dx, dy));
        setMagnitude(distanceBetween(dx, dy));
    }
    
    /**
    * Constructor using javax.vecmath.Point2d
    */
    public Vector(Point2d source, Point2d target){
    	double dx = target.x - source.x;
        double dy = source.y - target.y;
        setDirection(angleBetween(dx, dy));
        setMagnitude(distanceBetween(dx, dy));
    }

    /**
     * Create a vector that is identical to the given other vector.
     */
    public Vector (Vector other) {
        this(other.getDirection(), other.getMagnitude());
    }

    /**
     * Reset this vector to zero.
     */
    public void reset () {
        setDirection(0);
        setMagnitude(0);
    }

    /**
     * Returns this vector's magnitude (in pixels).
     */
    public double getMagnitude () {
        return myMagnitude;
    }

    /**
     * Returns this vector's magnitude relative to the given other vector.
     * 
     * More formally, returns the magnitude of this vector projected onto the
     * given other vector.
     */
    public double getRelativeMagnitude (Vector other) {
        return getMagnitude() * Math.cos(Math.toRadians(getAngleBetween(other)));
    }
    
    public double dotProduct(Vector other){
    	return getMagnitude() * other.getMagnitude() * Math.cos(Math.toRadians(getAngleBetween(other)));
    }
    
    /**
     * Scales this vector's magnitude by the given change value.
     * <UL>
     * <LI>A value of 1 leaves the magnitude unchanged
     * <LI>Values less than 1 reduce the magnitude
     * <LI>Values greater than 1 increase the magnitude
     * </UL>
     */
    public void scale (double change) {
        setMagnitude(getMagnitude() * change);
    }

    /**
     * Sets this vector's magnitude to the given value.
     */
    protected void setMagnitude (double value) {
        myMagnitude = value;
    }

    /**
     * Returns this vector's direction (in degrees).
     */
    public double getDirection () {
        // standardize between -360 and +360 (keep 360, -360, and 0 as distinct values)
        final double OFFSET = 0.001;
        double sign = (myAngle < 0) ? 1 : -1;
        return ((myAngle + sign * OFFSET) % 360) - sign * OFFSET;
    }

    /**
     * Returns the angle between this vector and the given other vector.
     */
    public double getAngleBetween (Vector other) {
        return getDirection() - other.getDirection();
    }

    /**
     * Adjusts this vector's direction by the given change value.
     */
    public void turn (double change) {
        setDirection(getDirection() + change);
    }

    /**
     * Sets this vector's direction to the given value.
     */
    protected void setDirection (double value) {
        myAngle = value;
    }

    /**
     * Returns the change in only the X direction represented by this vector.
     */
    public double getXChange () {
        return getMagnitude() * Math.cos(Math.toRadians(getDirection()));
    }

    /**
     * Returns the change in only the Y direction represented by this vector.
     */
    public double getYChange () {
        return getMagnitude() * Math.sin(Math.toRadians(getDirection()));
    }

    /**
     * Adds the given vector to this vector.
     */
    public void sum (Vector other) {
        // double a1 = getAngle();
        // double a2 = other.getAngle();
        // double m1 = getMagnitude();
        // double m2 = other.getMagnitude();
        // double speed = Math.sqrt(m1 * m1 + m2 * m2 + 2 * m1 * m2 *
        // Math.cos(Math.toRadians(a1 - a2)));
        // double angle = Math.toDegrees(Math.asin(m2 *
        // Math.sin(Math.toRadians(a2 - a1)) / speed)) + a1;
        // return new vector(angle, speed);

        // more readable, although slightly slower
        double dx = getXChange() + other.getXChange();
        double dy = getYChange() + other.getYChange();
        setDirection(angleBetween(dx, dy));
        setMagnitude(distanceBetween(dx, dy));
    }

    /**
     * Subtracts the given vector from this vector.
     */
    public void difference (Vector other) {
        // avoid changing other vector
        Vector v = new Vector(other);
        v.negate();
        sum(v);
    }

    /**
     * Returns a vector of the same magnitude, but in the opposite direction as
     * this vector.
     */
    public void negate () {
        turn(180);
    }

    /**
     * Returns the average of this vector with the given other vector.
     */
    public Vector average (Vector other) {
        return new Vector((getDirection() + other.getDirection()) / 2.0,
                          (getMagnitude() + other.getMagnitude()) / 2.0);
    }

    /**
     * Return true if this vector has the same magnitude and direction
     * as the given other vector.
     */
    @Override
    public boolean equals (Object vector) {
        try {
            Vector other = (Vector) vector;
            return (fuzzyEquals(getMagnitude(), other.getMagnitude()) && 
                    fuzzyEquals(getDirection(), other.getDirection()));
        }
        catch (ClassCastException e) {
            return false;
        }
    }

    /**
     * Returns this vector's values formatted as a string.
     */
    @Override
    public String toString () {
        return String.format("(%1.2f, %1.2f)", getDirection(), getMagnitude());
    }

    /**
     * Returns the distance between given two points
     */
    public static double distanceBetween (Point2D p1, Point2D p2) {
        return distanceBetween(p1.getX() - p2.getX(), p1.getY() - p2.getY());
    }

    /**
     * Returns the distance represented by the given dx and dy
     */
    public static double distanceBetween (double dx, double dy) {
        return Math.sqrt(dx * dx + dy * dy);
    }

    /**
     * Returns the angle between the given two points
     */
    public static double angleBetween (Point2D p1, Point2D p2) {
        return angleBetween(p1.getX() - p2.getX(), p1.getY() - p2.getY());
    }

    /**
     * Returns the angle represented by the given dx and dy
     */
    public static double angleBetween (double dx, double dy) {
        // TODO: this is still buggy :(
        return 180 - Math.toDegrees(Math.atan2(dy, dx));
    }

    /**
     * Returns true if two real values are approximately equal.
     * 
     * For more details, see
     * http://www.ibm.com/developerworks/java/library/j-jtp0114/#N10255
     * 
     * This function exists in many add-on libraries, but not standard Java :(
     */
    public static boolean fuzzyEquals (double a, double b) {
        // value based on this table:
        // http://en.wikipedia.org/wiki/Machine_epsilon#Values_for_standard_hardware_floating_point_arithmetics
        final double EPSILON = 5.96e-08;
        if (Double.isNaN(a) && Double.isNaN(b) || Double.isInfinite(a) && Double.isInfinite(b)) {
            return true;
        }
        else {
            return Math.abs(a / b - 1) < EPSILON;
        }
    }
}
