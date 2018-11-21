package uk.ac.cam.gurdon;
import java.awt.Rectangle;
import java.util.Arrays;

import ij.IJ;
import ij.gui.Roi;

/** Represents a 2-dimensional filopodium. Stores <code>Rois</code>, time and part indices, coordinates and statistics.
 * 
 * @author Richard Butler
 */
public class Filopart implements FiloPod{
	Roi roi,base,tip;
	int T;
	int index;
	double pixW,area,baseMean,projMean,tipMean,tipThMean,joinCost,dctm,dcbm,sigma,length;
	Point2d baseCoord, tipCoord;
	
	/** Used for testing, the non-static <code>Filopart</code> constuctor should be used instead.
	 * 
	 * @param r	The process <code>Roi</code>
	 * @param base	The process base <code>Roi</code>
	 * @param tip	The process tip <code>Roi</code>
	 * @param pixW	The calibrated pixel width
	 * @param t	The timepoint that this process exists at
	 * @param i	The index of this process
	 * @param a	The process area in units^2
	 * @param meanB The base mean intensity
	 * @param meanP The process mean intensity
	 * @param meanT The tip mean intensity
	 * @param meanThT The thresholded mean intensity
	 * @param sigma	The Gaussian sigma value used to detect this process
	 * @return the new <code>Filopart</code>
	 */
	@Deprecated
	public static Filopart create(Roi r, Roi base, Roi tip, double pixW, int t, int i, double a, double meanB, double meanP, double meanT, double meanThT, double sigma){
		Filopart part = null;
		try{
			part = new Filopart(r, base, tip, pixW, t, i, a, meanB, meanP, meanT, meanThT, sigma);
		}catch(Throwable bad){IJ.log(bad.toString()+"\n~~~~~\n"+Arrays.toString(bad.getStackTrace()).replace(",","\n"));}
		return part;
	}
	
	/** Constuctor taking <code>Roi</code>s and values to be stored
	 * 
	 * @param r	The process <code>Roi</code>
	 * @param base	The process base <code>Roi</code>
	 * @param tip	The process tip <code>Roi</code>
	 * @param pixW	The calibrated pixel width
	 * @param t	The timepoint that this process exists at
	 * @param i	The index of this process
	 * @param a	The process area in units^2
	 * @param meanB The base mean intensity
	 * @param meanP The process mean intensity
	 * @param meanT The tip mean intensity
	 * @param meanThT The thresholded mean intensity
	 * @param sigma	The Gaussian sigma value used to detect this process 
	 */
	public Filopart(Roi r, Roi base, Roi tip, double pixW, int t, int i, double a, double meanB, double meanP, double meanT, double meanThT, double sigma){
		try{
			this.roi = r;
			this.pixW = pixW;
			this.T = t;
			this.index = i;
			this.area = a;
			this.base = base;
			this.tip = tip;
			this.baseMean = meanB;
			this.projMean = meanP;
			this.tipMean = meanT;
			this.tipThMean = meanThT;
			Rectangle rectb = base.getBounds();
			this.baseCoord = new Point2d( (rectb.x+(rectb.width/2))*pixW, (rectb.y+(rectb.height/2))*pixW);
			Rectangle rectt = tip.getBounds();
			this.tipCoord = new Point2d( (rectt.x+(rectt.width/2))*pixW, (rectt.y+(rectt.height/2))*pixW);
			this.joinCost = Double.MAX_VALUE;
			this.dctm = 0d;
			this.dcbm = 0d;
			this.sigma = sigma;
			this.length = Double.NaN;
		}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Construct a <code>Filopart</code> containing the same <code>Roi</code>s and values as another.
	 * 
	 * @param copy	The <code>Filopart</code> to copy
	 */
	public Filopart(Filopart copy){
		try{
			this.roi = (Roi)copy.roi.clone();
			this.pixW = copy.pixW;
			this.T = copy.T;
			this.index = copy.index;
			this.area = copy.area;
			this.base = copy.base;
			this.tip = copy.tip;
			this.baseMean = copy.baseMean;
			this.projMean = copy.projMean;
			this.tipMean = copy.tipMean;
			this.tipThMean = copy.tipThMean;
			this.baseCoord = copy.baseCoord;
			this.tipCoord = copy.tipCoord;
			this.joinCost = copy.joinCost;
			this.dctm = copy.dctm;
			this.dcbm = copy.dcbm;
			this.sigma = copy.sigma;
			this.length = copy.length;
		}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Estimate the length of this filopodium as half perimeter corrected for width.
	 * <p>
	 * 	width factor w = 2 * sqrt(2*log(2)) * sigma<br>
	 *	estimate correction EC = ((PI - 2)/2) * w<br>
	 *	distance D = distance from the base to the tip<br>
	 *	scale factor SF = min( (D/w)^2, 1d )<br><br>
	 *	estimated length = perimeter/2 - EC * SF
	 * </p>
	 * 
	 * @return	The estimated length
	 */
	public double getLength(){
		try{
		if(Double.isNaN(length)){
			double w = 2 * Math.sqrt(2*Math.log(2)) * sigma * pixW;
			double EC = ((Math.PI - 2)/2) * w;
			double D = baseCoord.distance(tipCoord);
			double SF = Math.min( ((D/w)*(D/w)), 1d );
			length = ((roi.getLength()/2)*pixW) - (EC * SF);
		}
		}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return length;
	}

	@Override
	public Filopart getCopy() {
		return new Filopart(this);
	}

	@Override
	public Roi getRoi() {
		return roi;
	}

	@Override
	public Point2d getBaseCoord(){
		return baseCoord;
	}
	
	@Override
	public Point2d getTipCoord(){
		return tipCoord;
	}
	
	@Override
	public double baseDistance(FiloPod fp) {
		return this.getBaseCoord().distance(fp.getBaseCoord());
	}

	@Override
	public int getT() {
		return T;
	}

	@Override
	public int getIndex() {
		return index;
	}

	@Override
	public Roi getBase() {
		return base;
	}

	@Override
	public Roi getTip() {
		return tip;
	}

	@Override
	public double getArea() {
		return area;
	}

	@Override
	public double getBaseMean() {
		return baseMean;
	}

	@Override
	public double getProjMean() {
		return projMean;
	}

	@Override
	public double getTipMean() {
		return tipMean;
	}

	@Override
	public double getTipThMean() {
		return tipThMean;
	}

	@Override
	public double getJoinCost() {
		return joinCost;
	}

	@Override
	public double getDctm() {
		return dctm;
	}

	@Override
	public double getDcbm() {
		return dcbm;
	}

	@Override
	public double getSigma() {
		return sigma;
	}

	@Override
	public void setJoinCost(double cost) {
		this.joinCost = cost;
	}
	
	@Override
	public void setIndex(int index) {
		this.index = index;
	}

	@Override
	public void setDcbm(double value) {
		this.dcbm = value;
	}

	@Override
	public void setDctm(double value) {
		this.dctm = value;
	}
	
}
