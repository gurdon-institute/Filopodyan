package uk.ac.cam.gurdon;
import java.util.ArrayList;
import java.util.Arrays;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.ShapeRoi;
import ij.process.ImageStatistics;

/** A simple method to assign filopodium identity over time. A fast 1-step algorithm is used since the same cost for two links is very unlikely using this formula:<br>
 * cost = ((distance between bases + distance between tips) / sqrt(overlap area)) * time difference
 * 
 * @author Richard Butler
 */
public class OneStepAssigner implements LinearAssigner{
	private boolean verbose;

	/** 
	 * 	@param verbose	true to log additional information
 	 */
	public OneStepAssigner(boolean verbose){
		this.verbose = verbose;
	}
	
	/** Calculate the cost of joining two <code>FiloPod</code>s in this image.
	 * 	Takes into account the distance between the tips and bases of the processes, the area of overlap between them and the time difference:<br>
	 * 
	 * cost = ((distance between bases + distance between tips) / sqrt(overlap area)) * time difference
	 * 
	 */
	public double cost(FiloPod f1, FiloPod f2, ImagePlus imp){
		double cost = Double.POSITIVE_INFINITY;
	try{
		IJ.run(imp, "Select None", "");
		ShapeRoi intersection = new ShapeRoi(f1.getRoi()).and(new ShapeRoi(f2.getRoi()));
		imp.setRoi(intersection);
		if(imp.getRoi()==null){return Double.POSITIVE_INFINITY;}
		double overlap = imp.getStatistics(ImageStatistics.AREA).area;
		IJ.run(imp, "Select None", "");
		
		double gapScale = (double)f2.getT() - f1.getT();
		double dist1 = f1.baseDistance(f2);
		double dist2 = f1.baseDistance(f2);
		
		cost = ( (dist1+dist2)/Math.sqrt(overlap) )*gapScale;
		if(verbose){FilopodyanLog.get().print(imp.getTitle(), f1.getIndex()+" - "+f2.getIndex()+" cost = "+cost);}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return cost;
	}
	
	/** One step linear assignment.
	 * 
	 * @param filo	The <code>FiloPod</code> Collection for assignment. This is a List of timepoints each having a List of <code>FiloPod</code>s.
	 * @see Filopart
	 * @param imp	The <code>ImagePlus</code> that the <code>FiloPod</code>s came from
	 * @return The <code>FiloPod</code> Collection with track indices assigned for convenience
	 */
	public ArrayList<ArrayList<FiloPod>> assign(ArrayList<ArrayList<FiloPod>> filo, ImagePlus imp){
	try{
		int maxI = -1;
		int nFilo = filo.size();
		for(int t=0;t<nFilo;t++){
			if(verbose){FilopodyanLog.get().print(imp.getTitle(), "Linear assignment T"+t+"+");}
			if(filo.get(t).size()==0){
				FilopodyanLog.get().print(imp.getTitle(), imp.getTitle()+" - no filopodia at T"+t);
				continue;
			}
			
			for(int a=0;a<filo.get(t).size();a++){
				
				FiloPod partA = filo.get(t).get(a);
				partA.setJoinCost(Double.MAX_VALUE);
				double minCost = Double.MAX_VALUE;	//big number, but less than infinity (cost for rejected links)
				int minI = -1;
				maxI = Math.max(maxI,partA.getIndex());
				int gap = 1;
				if(t<nFilo-1){
					for(int b=0;b<filo.get(t+1).size();b++){
						FiloPod partB = filo.get(t+1).get(b);
						double cost = cost(partA,partB,imp);
						if(cost<minCost&&cost<partB.getJoinCost()){
							minCost = cost;
							minI = b;
							gap = 1;
						}
					}
				}
				if(t<nFilo-2&&minI==-1&&filo.get(t+2).size()>0){
					for(int c=0;c<filo.get(t+2).size();c++){
						FiloPod partC = filo.get(t+2).get(c);
						double cost = cost(partA,partC,imp);
						if(cost<minCost&&cost<partC.getJoinCost()){
							minCost = cost;
							minI = c;
							gap = 2;
						}
					}
				}
				if(minI>-1){
					filo.get(t+gap).get(minI).setIndex(partA.getIndex());
					filo.get(t+gap).get(minI).setJoinCost(minCost);
				}
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return filo;
	}
	
}
