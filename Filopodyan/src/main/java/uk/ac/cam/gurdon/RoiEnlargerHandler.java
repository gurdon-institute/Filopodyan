package uk.ac.cam.gurdon;
import ij.gui.Roi;
import ij.plugin.RoiEnlarger;

/** Wrapper to conveniently work around bug in ij.plugin.filter.ThresholdToSelection caused by trying to convert threshold to ShapeRoi with a mask that has no signal
 * @author Richard Butler
 */
public class RoiEnlargerHandler {
	
	/** Calls the RoiEnlarger enlarge method and returns the result or the input Roi if an ArrayIndexOutOfBoundsException is thrown
	 * 
	 * @param in	The Roi to be scaled
	 * @param scale	The scale amount in pixels, positive to grow or negative to shrink
	 * @return	The scaled Roi or the input Roi if an ArrayIndexOutOfBoundsException is thrown
	 */
	public static Roi enlarge(Roi in, int scale){
		Roi roi = null;
		boolean tooSmall = false;
		try{
			roi = RoiEnlarger.enlarge(in, scale);
		}catch(ArrayIndexOutOfBoundsException oob){
			tooSmall = true;	//if oob was thrown, the erosion completely removed the Roi
		}
		if(!tooSmall){
			return roi;
		}
		else{
			return in;
		}
	}
	
	/** Run a binary open operation on a Roi without requiring a binary mask by running sequential shrink and enlarge operations
	 * 
	 * @param in	The Roi to be processed
	 * @param scale	The scale in pixels
	 * @return The opened Roi
	 */
	public static Roi open(Roi in, int scale){
		Roi eroded = enlarge(in, -scale);
		Roi dilated = enlarge(eroded, scale);
		return dilated;
	}
	
	/** Run a binary close operation on a Roi without requiring a binary mask by running sequential enlarge and shrink operations
	 * 
	 * @param in	The Roi to be processed
	 * @param scale	The scale in pixels
	 * @return The closeded Roi
	 */
	public static Roi close(Roi in, int scale){
		Roi dilated = enlarge(in, scale);
		Roi eroded = enlarge(dilated, -scale);
		return eroded;
	}
	
}
