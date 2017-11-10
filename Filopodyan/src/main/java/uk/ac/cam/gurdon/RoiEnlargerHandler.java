package uk.ac.cam.gurdon;
import ij.gui.Roi;
import ij.plugin.RoiEnlarger;

/* wrapper to conveniently work around bug in ThresholdToSelection caused by trying to convert threshold to ShapeRoi with a mask that has no signal
 */
public class RoiEnlargerHandler {
	
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
	
	public static Roi open(Roi in, int scale){
		Roi eroded = enlarge(in, -scale);
		Roi dilated = enlarge(eroded, scale);
		return dilated;
	}
	
	public static Roi close(Roi in, int scale){
		Roi dilated = enlarge(in, scale);
		Roi eroded = enlarge(dilated, -scale);
		return eroded;
	}
	
}
