package uk.ac.cam.gurdon;

import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.Overlay;
import ij.gui.PolygonRoi;
import ij.gui.Roi;
import ij.measure.Calibration;
import ij.plugin.Straightener;
import ij.plugin.filter.EDM;
import ij.plugin.filter.MaximumFinder;
import ij.process.Blitter;
import ij.process.ByteProcessor;
import ij.process.FloatPolygon;
import ij.process.FloatProcessor;
import ij.process.ImageProcessor;
import ij.process.ShortProcessor;

public class ProcessProfiler {
	private FilopodyanGui bgui;
	private ImagePlus imp;
	private Overlay ol;
	
	/**
	 * @param bgui	The FilopodyanGui containing parameters
	 * @param imp	The ImagePlus from which Filoparts to be visualised were acquired
	 */
	public ProcessProfiler(FilopodyanGui bgui, ImagePlus imp, Overlay ol) {
		this.bgui = bgui;
		this.imp = imp;
		this.ol = ol;
	}

	/** custom sort to order points from base to tip
	 *  sort by distance ratio   p to start:p to end
	 */
	private class LineOrder implements Comparator<Point2d>{
		private Point2d start, end;
		
		private LineOrder(Point2d start, Point2d end){
			this.start = start;
			this.end = end;
		}
		
		@Override
		public int compare(Point2d p1, Point2d p2) {
			double startD1 = p1.distance(start);
			double endD1 = p1.distance(end);
			double startD2 = p2.distance(start);
			double endD2 = p2.distance(end);
			double pos = startD1/endD1 - startD2/endD2;
			int order = 0;
			if(pos>0){
				order = 1;
			}
			else if(pos<0){
				order = -1;
			}
			return order;
		}
		
	}
	
	/** Analyse <code>FiloPod</code> intensity profiles over time
	 * 
	 * @param filo	The Filopart Collection to be analysed. This is a List of timepoints each having a List of FiloParts.
	 */
	public void run(ArrayList<ArrayList<FiloPod>> filo){
		try{
			HashMap<Integer,ArrayList<FiloPod>> tracks = new HashMap<Integer,ArrayList<FiloPod>>();
			for(ArrayList<FiloPod> timePointList: filo){
				for(FiloPod filopodium : timePointList){
					int i = filopodium .getIndex();
					if(!tracks.containsKey(i)){
						tracks.put(i, new ArrayList<FiloPod>());
					}
					tracks.get(i).add( filopodium );
				}
			}
			
			Calibration cal = imp.getCalibration();
			Straightener straightener = new Straightener();
			int extraspace = 200;	//fixed extra width to allow offset by DCBM, cropped to required size later
			int minX = Integer.MAX_VALUE;
			int maxX = Integer.MIN_VALUE;
			HashMap<Integer, FloatProcessor> profileGraphs = new HashMap<Integer, FloatProcessor>();
			for(Integer tracki:tracks.keySet()){
				HashMap<Integer, ImageProcessor> trackProfiles = new HashMap<Integer, ImageProcessor>();
				int moveSum = 0;	//total DCBM to apply offset for profile registration
				//System.out.println("track "+tracki);
				for(FiloPod fp:tracks.get(tracki)){
					PolygonRoi line = getProfileLine(fp);
					if (imp.isHyperStack()||imp.isComposite()) {
						line.setPosition(0, 0, fp.getT());
			        }
			        else{
			        	line.setPosition(fp.getT());
			        }
					
					ImageProcessor ip = imp.getStack().getProcessor( imp.getStackIndex(bgui.measureC, 1, fp.getT()) );
					ImagePlus wrapper = new ImagePlus("wrapper",ip);
					wrapper.setRoi(line);
					ImageProcessor straight = straightener.straighten(wrapper, line, bgui.eds); //line width from ed iteration count
					
					int dcbmPx =  (int)(fp.getDcbm()/cal.pixelWidth);	//base movement can be +ve or -ve
					int totalLength = straight.getWidth() + 2*extraspace;	//add extra space at each side for base movement offset
					ImageProcessor straightOffset = null;
					if(imp.getBitDepth()==8){
						straightOffset = new ByteProcessor(totalLength, imp.getNFrames());
					}
					else if(imp.getBitDepth()==16){
						straightOffset = new ShortProcessor(totalLength, imp.getNFrames());
					}
					else{
						straightOffset = new FloatProcessor(totalLength, imp.getNFrames());
					}
					moveSum += dcbmPx;
					minX = Math.min(minX, extraspace+moveSum);
					maxX = Math.max(maxX, straight.getWidth()+extraspace+moveSum);
					straightOffset.copyBits(straight, extraspace+moveSum,0, Blitter.COPY);	//offset according to base movement

					trackProfiles.put(fp.getT(), straightOffset);
					
					if(ol!=null){	//show profile lines in the Overlay if one was passed to constructor
						ol.add(line);
						/*FloatPolygon poly = line.getFloatPolygon();
						OvalRoi start = new OvalRoi(poly.xpoints[0]-1,poly.ypoints[0]-1, 3,3);
						start.setStrokeColor(Color.RED);
						start.setPosition(0,0,fp.getT());
						ol.add(start);
						OvalRoi end = new OvalRoi(poly.xpoints[poly.npoints-1]-1,poly.ypoints[poly.npoints-1]-1, 3,3);
						end.setStrokeColor(Color.GREEN);
						end.setPosition(0,0,fp.getT());
						ol.add(end);*/
					}
				}
				
				profileGraphs.put(tracki, kymoProfile(trackProfiles) );
				
			}
			
			int cropW = maxX-minX+2;
			for(int tracki:profileGraphs.keySet()){
				FloatProcessor graph = profileGraphs.get(tracki);
				FloatProcessor cropped = new FloatProcessor(cropW,imp.getNFrames());
				cropped.copyBits(graph, -minX+1,0, Blitter.COPY);
				profileGraphs.put(tracki, cropped);
			}
			
			Calibration distTimeCal = new Calibration();
			distTimeCal.pixelWidth = cal.pixelWidth;
			distTimeCal.pixelHeight = cal.frameInterval;
			distTimeCal.setUnit(cal.getUnit());
			distTimeCal.setTimeUnit(cal.getTimeUnit());
			
			new ProcessGraphWindow(profileGraphs, distTimeCal).display();
			
		}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}

	private PolygonRoi getProfileLine(FiloPod filopod) {
		Roi roi = filopod.getRoi();
		Rectangle offset = roi.getBounds();
		
		Point2d base = filopod.getBaseCoord();
		Point2d tip = filopod.getTipCoord();
		
		//get ultimate points
		EDM edm = new EDM();
		FloatProcessor processEdm = edm.makeFloatEDM(roi.getMask(), 0, false);
		MaximumFinder mf = new MaximumFinder();
		Polygon uepPolygon = mf.getMaxima(processEdm, 0.0, true);
		
		//collect all ultimate eroded points (Euclidean distance map maxima) as a FloatPolygon, prepend the base and append the tip coordinates
		ArrayList<Point2d> points = new ArrayList<Point2d>();
		Calibration cal = imp.getCalibration();	
		points.add(new Point2d(base.x/cal.pixelWidth, base.y/cal.pixelHeight)); //tip and base coords are calibrated and for the whole image bounds
		for(int p=0;p<uepPolygon.npoints;p++){
			points.add(new Point2d(uepPolygon.xpoints[p]+offset.x, uepPolygon.ypoints[p]+offset.y)); //UEP coords are uncalibrated and for the FiloPod mask bounds
		}
		points.add(new Point2d(tip.x/cal.pixelWidth, tip.y/cal.pixelHeight));
		
		//sort points into order of increasing distance from base and decreasing distance to tip
		Collections.sort(points, new LineOrder(points.get(0), points.get(points.size()-1)));
		
		FloatPolygon poly = new FloatPolygon();
		for(Point2d p:points){
			poly.addPoint(p.x, p.y);
		}
		PolygonRoi processLine = new PolygonRoi(poly, PolygonRoi.POLYLINE);
		
		return processLine;
	}
	
	private FloatProcessor kymoProfile(final HashMap<Integer, ImageProcessor> profiles){
		int maxLen = profiles.keySet().stream().mapToInt(id->profiles.get(id).getWidth()).max().getAsInt();
		int nFrames = imp.getNFrames();
		
		FloatProcessor graph = new FloatProcessor(maxLen, nFrames); //length of longest profile * number of timepoints
		//graph.setValue(-1f);
		//graph.fill();	//set position,time with no value to -1
		for(int t:profiles.keySet()){	//for each timepoint in this track
			ImageProcessor ip = profiles.get(t);
			for(int x=0;x<ip.getWidth();x++){	//along the profile length
				//float sum = 0f;
				float max = -1f;
				int H = ip.getHeight();
				for(int y=0;y<H;y++){
					float f = ip.getf(x,y);
					//sum += f;
					max = Math.max(max, f);
				}
				//graph.setf(x,t-1, sum/(float)H);	//store the mean across the line width at this position
				graph.setf(x,t-1, max);	//store the max across the line width at this position
			}
		}
		return graph;
	}
	
}
