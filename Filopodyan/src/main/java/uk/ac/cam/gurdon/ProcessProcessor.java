package uk.ac.cam.gurdon;
import java.awt.Color;
import java.awt.Polygon;
import java.util.Arrays;

import ij.IJ;
import ij.ImagePlus;
import ij.Prefs;
import ij.gui.Line;
import ij.gui.Roi;
import ij.gui.ShapeRoi;
import ij.process.ImageProcessor;

/** Joins fragmented parts into a continuous binary mask
 * 
 * @author Richard Butler
 */
public class ProcessProcessor{
private static final int MAX_ITERATIONS = 10;

	private class Tople{
		public Line line;
		public double cost;
		
		public Tople(){
			
		}
		
		public Tople(Line line,double d){
			this.line = line;
			this.cost = d;
		}
		
		public String toString(){
			return line.toString()+" , cost = "+cost;
		}
		
	}
	
	private Tople joinCost(Roi a,Roi b){
		Tople result = new Tople();
	try{
		Polygon polyA = a.getPolygon();
		Polygon polyB = b.getPolygon();
	 	
		double minCost = Double.POSITIVE_INFINITY;
		int mina = -1;
		int minb = -1;
		for(int ia=0;ia<polyA.npoints;ia++){
			for(int ib=0;ib<polyB.npoints;ib++){
				double sqdist = ((polyA.xpoints[ia]-polyB.xpoints[ib])*(polyA.xpoints[ia]-polyB.xpoints[ib])) 
							  + ((polyA.ypoints[ia]-polyB.ypoints[ib])*(polyA.ypoints[ia]-polyB.ypoints[ib]));
				double cost = sqdist;
				if(cost<minCost){
					minCost = cost;
					mina = ia;
					minb = ib;
				}
			}
		}
		result = new Tople(new Line(polyA.xpoints[mina],polyA.ypoints[mina],polyB.xpoints[minb],polyB.ypoints[minb]),minCost);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return result;
	}
	
	/** Attempts to joins fragments in the input binary mask into a continuous mask by adding lines between the nearest points on the fragment boundaries
	 * 
	 *  @param map	the input mask, will be processed in place
	 *  @return	the input map after processing (for convenience)
	 */
	public ImagePlus join(ImagePlus map){
		ImageProcessor ip = map.getProcessor();
		ip.setColor(Color.WHITE);
		for(int t=1;t<=map.getNFrames();t++){
			map.setPosition(1,1,t);
			int n = -1;
			int count = -1;
			while(true){
				n++;
				IJ.run(map, "Create Selection", "");
				if(map.getStatistics().mean==0){
					IJ.run(map, "Make Inverse","");
				}
				Roi[] roi = new ShapeRoi(map.getRoi()).getRois();
				if(roi.length==1||roi.length==count){	//stop if all areas are joined or no more areas were joined in the last iteration
					break;
				}
				count = roi.length;
				if(n==MAX_ITERATIONS){
					FilopodyanLog.get().print(map.getTitle(), "T"+t+" fragment joining did not converge after "+MAX_ITERATIONS+" iterations");
					break;
				}
				double bigl = Double.NEGATIVE_INFINITY;
				for(int b=0;b<roi.length;b++){
					double length = roi[b].getLength();
					if(length>bigl){
						bigl = length;
					}
				}
				
				for(int p=0;p<roi.length;p++){
					ij.gui.Line minLine = new Line(0,0,0,0);
					double minCost = Double.POSITIVE_INFINITY;
	
					for(int i=0;i<roi.length;i++){
						if(i==p){continue;}
						Roi roiA = roi[p];
						Roi roiB = roi[i];
						if(roiB.getLength()>roiA.getLength()){	//make sure roiA is always the largest
							roiA = roi[i];
							roiB = roi[p];
						}
						Tople tople = joinCost(roiA,roiB);
						if(tople.cost<minCost){
							minCost = tople.cost;
							minLine = tople.line;
						}
					}
					if(minCost==Double.POSITIVE_INFINITY){continue;}
					map.setRoi(minLine);
					if(minLine.getLength()==0){continue;}	//don't join if the line doesn't exist
					IJ.run(map, "Line to Area", "");
					Roi fillRoi = map.getRoi();
					fillRoi = RoiEnlargerHandler.enlarge(fillRoi,2);	//make longer as well as setting a width
					ip.fill(fillRoi);
				}
			
				IJ.run(map, "Create Selection", "");
				map.killRoi();

			}
		}
		Prefs.blackBackground = true;
		IJ.run(map, "Convert to Mask", "method=Default background=Default black");
		return map;
	}

	
}
