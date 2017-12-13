package uk.ac.cam.gurdon;

import ij.gui.Roi;

public interface FiloPod {

	public double getLength();
	
	public FiloPod getCopy();
	
	public Point2d getBaseCoord();
	
	public Point2d getTipCoord();
	
	public int getT();
	
	public int getIndex();
	
	public Roi getRoi();
	public Roi getBase();
	public Roi getTip();

	public double getArea();
	public double getBaseMean();
	public double getProjMean();
	public double getTipMean();
	public double getTipThMean();
	public double getJoinCost();
	public double getDctm();
	public double getDcbm();
	public double getSigma();
	
	public void setJoinCost(double cost);
	public void setIndex(int index);
	public void setDcbm(double value);
	public void setDctm(double value);
	
	public double baseDistance(FiloPod fp);
	
}
