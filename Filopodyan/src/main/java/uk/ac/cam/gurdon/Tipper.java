package uk.ac.cam.gurdon;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.Arrays;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.OvalRoi;
import ij.gui.Roi;
import ij.gui.ShapeRoi;
import ij.plugin.Duplicator;

/** Finds the most likely tip positions of non-branched processes.
 * 
 * @author Richard Butler
 */
public class Tipper{
private static final int MIN_R = 3;
	
	/** Finds a process tip based on distance from the base and optionally signal maxima in the specified channel.
	 * 
	 * @param imp	The image containing the process
	 * @param processRoi	The Roi representing the process
	 * @param base	The coordinates of the process base
	 * @param measureC	The channel index for intensity measurement
	 * @param time	The time index of the process
	 * @param fit	true if the tip position should be adjusted based on maxima in the measurement channel, false otherwise
	 * @return	The <code>Roi</code> representing the process tip
	 */
	public Roi findTip(ImagePlus imp,Roi processRoi,Point base,int measureC,int time,boolean fit){
		Roi tipRoi = new Roi(0,0,0,0);
		Polygon process = processRoi.getPolygon();
	try{
			int maxI = -1;
			double maxED = Double.NEGATIVE_INFINITY;
			for(int p=0;p<process.npoints;p++){
				double ed = ((base.x-process.xpoints[p])*(base.x-process.xpoints[p])) + ((base.y-process.ypoints[p])*(base.y-process.ypoints[p]));
				if( ed>maxED ){
					maxED = ed;
					maxI = p;
				}
			}
			if(fit){
				imp.setRoi(processRoi);
				int area = imp.getStatistics().pixelCount;
				int r = (int)Math.round(Math.sqrt((area/2)/Math.PI));
				if(r<MIN_R){r=MIN_R;}
				else if(r>20){r=20;}
				Roi tipRoi0 = new OvalRoi(process.xpoints[maxI]-r,process.ypoints[maxI]-r,2*r,2*r);
				imp.setPosition(measureC,1,time);
				imp.setRoi(tipRoi0);
				Rectangle rect = tipRoi0.getBounds();
				ImagePlus blur = new Duplicator().run(imp, measureC, measureC, 1, 1, time, time);
				IJ.run(blur, "Gaussian Blur...", "sigma=1");
				IJ.run(blur, "Find Maxima...", "noise=25 output=[Point Selection] exclude");
				
				if(blur.getRoi()!=null){
					Point point = new Point(process.xpoints[maxI],process.ypoints[maxI]);
					Polygon poly = blur.getRoi().getPolygon();
					poly.addPoint(process.xpoints[maxI]-rect.x,process.ypoints[maxI]-rect.y);
					int n = poly.npoints;
					int wn = 0;
					for(int p=0;p<n;p++){
						int w = blur.getPixel(poly.xpoints[p],poly.ypoints[p])[0];	//intensity weighting for mean of maxima coordinates
						if(p==0){point=new Point(poly.xpoints[p]*w,poly.ypoints[p]*w);}
						else{
							point.x += (poly.xpoints[p]*w);
							point.y += (poly.ypoints[p]*w);
						}
						wn += w;
					}
					point.x = Math.round(point.x/wn);
					point.y = Math.round(point.y/wn);
					double mse = 0d;
					for(int p=0;p<poly.npoints;p++){
						mse += ((point.x-poly.xpoints[p])*(point.x-poly.xpoints[p])) + ((point.y-poly.ypoints[p])*(point.y-poly.ypoints[p]));
					}
					mse /= n;
					r = (int)Math.round(Math.sqrt(mse)/2);	//roi radius = half of root mean squared error
					if(r<MIN_R){r=MIN_R;}
					tipRoi = new OvalRoi(rect.x+point.x-r,rect.y+point.y-r,2*r,2*r);
				}
				else{
					r = MIN_R;
					tipRoi = new OvalRoi(process.xpoints[maxI]-r,process.ypoints[maxI]-r,2*r,2*r);
				}
				blur.close();
			}
			else{
				tipRoi = new OvalRoi(process.xpoints[maxI]-6,process.ypoints[maxI]-6,12,12);
				tipRoi = new ShapeRoi(tipRoi).and(new ShapeRoi(processRoi));
			}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return tipRoi;
	}
	
}
