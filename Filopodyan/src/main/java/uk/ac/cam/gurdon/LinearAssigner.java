package uk.ac.cam.gurdon;

import java.util.ArrayList;

import ij.ImagePlus;

/** Assigns filopodium identity over time. See <a href='https://imagej.net/TrackMate'>TrackMate</a> before implementing this!
 * 
 * @author Richard Butler
 */
public interface LinearAssigner {
	
	/** Assign track indexes to the passed <code>ArrayList<ArrayList<FiloPod>></code>
	 * 
	 * @param filo	The <code>FiloPod</code> Collection for assignment. This is a List of timepoints each having a List of <code>FiloPod</code>s.
	 * @see Filopart
	 * 
	 * @param imp	The <code>ImagePlus</code> that the <code>FiloPod</code>s came from
	 * 
	 * @return the <code>ArrayList<ArrayList<FiloPod>></code> with indexes assigned for convenience
	 */
	public ArrayList<ArrayList<FiloPod>> assign(ArrayList<ArrayList<FiloPod>> filo, ImagePlus imp);
	
	/** Compute and return the cost of joining two <code>FiloPod</code>s in this image
	 */
	public double cost(FiloPod f1, FiloPod f2, ImagePlus imp);
	
}
