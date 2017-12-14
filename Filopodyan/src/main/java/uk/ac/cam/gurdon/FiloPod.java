package uk.ac.cam.gurdon;

import ij.gui.Roi;

/** Implemented by objects representing Filopodia
 * 
 * @author Richard Butler
 */
public interface FiloPod {
	
	/** @return the process length
	 */
	public double getLength();
	
	/** @return a copy of this <code>FiloPod</code>
	 */
	public FiloPod getCopy();
	
	/** @return the process base coordinate
	 */
	public Point2d getBaseCoord();
	
	/** @return the process tip coordinate
	 */
	public Point2d getTipCoord();
	
	/** @return the time index 
	 */
	public int getT();
	
	/** @return the object index 
	 */
	public int getIndex();
	
	/** @return the process <code>Roi</code>
	 */
	public Roi getRoi();
	
	/** @return the process base <code>Roi</code>
	 */
	public Roi getBase();
	
	/** @return the process tip <code>Roi</code>
	 */
	public Roi getTip();

	/** @return the process area
	 */
	public double getArea();
	
	/** @return the base mean intensity
	 */
	public double getBaseMean();
	
	/** @return the process mean intensity
	 */
	public double getProjMean();
	
	/** @return the tip mean intensity
	 */
	public double getTipMean();
	
	/** @return the tip thresholded mean intensity
	 */
	public double getTipThMean();
	
	/** @return the join cost assigned to this object
	 */
	public double getJoinCost();
	
	/** @return the Direction Corrected Tip Movement from the previous to the current timepoint
	 */
	public double getDctm();
	
	/** @return the Direction Corrected Base Movement from the previous to the current timepoint
	 */
	public double getDcbm();
	
	/** @return the sigma value used for detection of this object
	 */
	public double getSigma();
	
	/** assign a join cost
	 * @param cost the value to assign
	 */
	public void setJoinCost(double cost);
	
	/** set the object index
	 * @param index the index to assign
	 */
	public void setIndex(int index);
	
	/** set the DCBM
	 * @param value the DCBM value
	 */
	public void setDcbm(double value);
	
	/** set the DCTM
	 * @param value the DCTM value
	 */
	public void setDctm(double value);
	
	/** @return the distance between the base coordinates of this object and the FiloPod argument
	 * @param fp the <code>FiloPod</code> to get base distance to
	 */
	public double baseDistance(FiloPod fp);
	
}
