package uk.ac.cam.gurdon;
import java.util.ArrayList;
import java.util.Arrays;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.ShapeRoi;
import ij.process.ImageStatistics;

/** A simple method to assign filopodium identity over time
 * 
 * @author Richard Butler
 */
public class LinearAssigner{
private ImagePlus imp;
private int maxI;
private boolean verbose;

	/** @param imp	the image from which the filopodia were mapped
	 * 	@param verbose	true to log additional information
 	 */
	public LinearAssigner(ImagePlus imp,boolean verbose){
		this.imp = imp;
		imp.getNFrames();
		this.verbose = verbose;
	}

	/** Run the linear asignment algorithm. A fast 1-step algorithm is used since the same cost for two links is very unlikely using the formula:
	 * cost = ((distance between bases + distance between tips) / sqrt(overlap area)) * time difference
	 * 
	 * Links are assigned by setting the Filopart track index fields
	 * 
	 * @param filo	The <code>Filopart</code> Collection for assignment. This is a List of timepoints each having a List of <code>FiloPart</code>s.
	 * @see Filopart
	 * @return The <code>Filopart</code> Collection with track indices assigned for convenience
	 */
	public ArrayList<ArrayList<Filopart>> run(ArrayList<ArrayList<Filopart>> filo){
		assign(filo);
		return filo;
	}
	
	private double cost(Filopart f1, Filopart f2, ImagePlus imp){
		double cost = Double.POSITIVE_INFINITY;
	try{
		IJ.run(imp, "Select None", "");
		ShapeRoi intersection = new ShapeRoi(f1.roi).and(new ShapeRoi(f2.roi));
		imp.setRoi(intersection);
		if(imp.getRoi()==null){return Double.POSITIVE_INFINITY;}
		double overlap = imp.getStatistics(ImageStatistics.AREA).area;
		IJ.run(imp, "Select None", "");
		
		double gapScale = (double)f2.T - f1.T;
		double dist1 = f1.baseCoord.distance(f2.baseCoord);
		double dist2 = f1.tipCoord.distance(f2.tipCoord);
		
		cost = ( (dist1+dist2)/Math.sqrt(overlap) )*gapScale;
		if(verbose){FilopodyanLog.get().print(imp.getTitle(), f1.index+" - "+f2.index+" cost = "+cost);}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return cost;
	}
	
	private void assign(ArrayList<ArrayList<Filopart>> filo){
	try{
		maxI = -1;
		int nFilo = filo.size();
		for(int t=0;t<nFilo;t++){
			if(verbose){FilopodyanLog.get().print(imp.getTitle(), "Linear assignment T"+t+"+");}
			if(filo.get(t).size()==0){
				FilopodyanLog.get().print(imp.getTitle(), imp.getTitle()+" - no filopodia at T"+t);
				continue;
			}
			
			for(int a=0;a<filo.get(t).size();a++){
				
				Filopart partA = filo.get(t).get(a);
				partA.joinCost = Double.MAX_VALUE;
				double minCost = Double.MAX_VALUE;	//big number, but less than infinity (cost for rejected links)
				int minI = -1;
				maxI = Math.max(maxI,partA.index);
				int gap = 1;
				if(t<nFilo-1){
					for(int b=0;b<filo.get(t+1).size();b++){
						Filopart partB = filo.get(t+1).get(b);
						double cost = cost(partA,partB,imp);
						if(cost<minCost&&cost<partB.joinCost){
							minCost = cost;
							minI = b;
							gap = 1;
						}
					}
				}
				if(t<nFilo-2&&minI==-1&&filo.get(t+2).size()>0){
					for(int c=0;c<filo.get(t+2).size();c++){
						Filopart partC = filo.get(t+2).get(c);
						double cost = cost(partA,partC,imp);
						if(cost<minCost&&cost<partC.joinCost){
							minCost = cost;
							minI = c;
							gap = 2;
						}
					}
				}
				if(minI>-1){
					filo.get(t+gap).get(minI).index = partA.index;
					filo.get(t+gap).get(minI).joinCost = minCost;
				}
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
}
