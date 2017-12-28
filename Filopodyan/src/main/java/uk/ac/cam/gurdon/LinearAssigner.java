package uk.ac.cam.gurdon;

import java.util.ArrayList;

import ij.ImagePlus;

/** Assigns filopodium identity over time. See <a href='https://imagej.net/TrackMate'>TrackMate</a> before implementing this!
 * 
 * @author Richard Butler
 */
public interface LinearAssigner {
	
	/** Assign track indexes to the passed <code>ArrayList&lt;ArrayList&lt;FiloPod&gt;&gt;</code><br>
	 * Links are assigned by setting the FiloPod track index fields.
	 * @param filo	The <code>FiloPod</code> Collection for assignment. This is a List of timepoints each having a List of <code>FiloPod</code>s.
	 * @see Filopart
	 * 
	 * @param imp	The <code>ImagePlus</code> that the <code>FiloPod</code>s came from
	 * 
	 * @return the same <code>ArrayList&lt;ArrayList&lt;FiloPod&gt;&gt;</code> passed as filo with indexes assigned for convenience
	 */
	public ArrayList<ArrayList<FiloPod>> assign(ArrayList<ArrayList<FiloPod>> filo, ImagePlus imp);
	
	/** Compute and return the cost of joining two <code>FiloPod</code>s in this image
	 * 
	 * @param f1	<code>FiloPod</code> for joining
	 * @param f2	<code>FiloPod</code> for joining
	 * @param imp	the image containing these <code>FiloPod</code>s
	 * @return the cost of joining these two <code>FiloPod</code>s
	 */
	public double cost(FiloPod f1, FiloPod f2, ImagePlus imp);
	
}
