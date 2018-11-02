package uk.ac.cam.gurdon;
import java.awt.Color;
import java.awt.Font;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import org.scijava.command.Command;
import org.scijava.plugin.Plugin;

import ij.IJ;
import ij.ImageJ;
import ij.ImagePlus;
import ij.Prefs;
import ij.WindowManager;
import ij.gui.OvalRoi;
import ij.gui.Overlay;
import ij.gui.PointRoi;
import ij.gui.Roi;
import ij.gui.ShapeRoi;
import ij.gui.TextRoi;
import ij.measure.Calibration;
import ij.measure.ResultsTable;
import ij.plugin.Duplicator;
import ij.plugin.HyperStackConverter;
import ij.plugin.ImageCalculator;
import ij.process.ByteProcessor;
import ij.process.ImageStatistics;

/**
 * An ImageJ PlugIn for filopodia dynamics analysis.
 *
 * For the accompanying manuscript, see:
 * <a href='http://jcb.rupress.org/content/216/10/3405'>Urbancic, V., Butler, R., Richier, B., Peter, M., Mason, J., Holt, C. E., Gallop, J. L. 2017. Filopodyan: An Open-Source Pipeline For The Analysis Of Filopodia.</a> DOI: 10.1083/jcb.201705113
 *
 * @author Richard Butler
 */

@Plugin(type = Command.class, menuPath = "Plugins>Filopodyan")
public class Filopodyan_ implements Command{
	public ImagePlus imp;
	private ImagePlus map,body,proj;
	public FilopodyanGui bgui;
	public int W,H,C,Z,T,ind,tStart,tEnd,firstTrackIndex;
	public String title,sanTitle;
	private String unit;
	private double pixelW;
	private ResultsTable filoRT, bodyRT, coordRT;
	private Duplicator dup = new Duplicator();
	private ArrayList<ArrayList<FiloPod>> filo;
	private ArrayList<Roi> backRoi;
	private Overlay ol;
	private ShapeRoi[] bodyRoiArr;
	private Roi[] frameBackgroundRoiArray;
	private Roi[] boundaryBackgroundRoiArray;
	private ArrayList<Roi> localBackgroundRois;
	private double[] bodyMean;
	private static final double defaultPixelW = 0.065; //Vasja's 63x objective
	private static final Font labelFont = new Font(Font.MONOSPACED,Font.BOLD,14);
	public boolean batch = false;
	private static final Color frameBackgroundColor = new Color(0, 0, 255, 32);
	private static final Color boundaryBackgroundColor = new Color(255, 0, 0, 32);
	private static final Color localBackgroundColor = new Color(0, 255, 0, 32);


	/** Sets visibility of the current image and runs Enhance Contrast if it is visible. Does nothing in batch mode.
	 * 
	 * @param v	 true to show, false to hide
	 */
	public void setImageVisible(final boolean v){
	try{
		if(batch){return;}
		imp.getWindow().setVisible(v);
		if(v)IJ.run(imp, "Enhance Contrast...", "saturated=0.4");
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Sets the image to be the target of all other operations.
	 * 
	 * @param image 	The target ImagePlus
	 */
	public void setImp(ImagePlus image){
		imp = image;
		title = imp.getTitle();
		sanTitle = title.replaceAll("[^a-zA-Z0-9_]","_");
		W = imp.getWidth();
		H = imp.getHeight();
		C = imp.getNChannels();
		Z = imp.getNSlices();
		T = imp.getNFrames();
		
		//imp = HyperStackConverter.toHyperStack(imp, C, Z, T); //ensure default XYCZT dimension order
		
		Calibration cal = imp.getCalibration();
		pixelW = cal.pixelWidth;
		unit = imp.getCalibration().getUnit();
		if(pixelW==1){
				cal.pixelWidth = defaultPixelW;
				cal.pixelHeight = defaultPixelW;
				imp.setCalibration(cal);
		}
		cal.getTimeUnit();
		
		if(unit.matches("[Mm]icrons?")){unit="\u00B5m";}
	}
	
	private void maxAreaOnly(ImagePlus target, int t0, int t1){
	try{
		for(int t=t0;t<=t1;t++){
			target.setPosition(1,1,t);
			IJ.run(target, "Create Selection", "");
			if(target.getStatistics().mean==0&&target.getRoi()!=null){
				IJ.run(target, "Make Inverse", "");
			}
			Roi br = target.getRoi();
			if(br==null){continue;}
			Roi[] split = new ShapeRoi(br).getRois();
			double maxA = -1d;
			int maxI = -1;
			for(int i=0;i<split.length;i++){
				target.setRoi(split[i]);
				double area = target.getStatistics().area;
				if(area>maxA){maxA=area;maxI=i;}
			}
			if(maxI>-1){
				target.setRoi(split[maxI]);
				IJ.setBackgroundColor(0, 0, 0);
				IJ.run(target, "Clear Outside", "slice");
			}
		}
			IJ.run(target, "Select None", "");
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}

	private boolean onEdge(Roi roi){
		int maxWH = Math.max(W,H);
		for(int xy=0; xy<maxWH; xy++){
			if(roi.contains(xy,0) || roi.contains(xy,H-1) || roi.contains(0,xy) || roi.contains(W-1,xy) ){
				return true;
			}
		}
		return false;
	}
	
	//set the position of a Roi using 1 dimension for a stack or 3 dimensions for a hyperstack
	/*private void setRoiFrame(Roi roi, int t){
		//System.out.println(C);
		if(C==1){
			int index = imp.getStackIndex(1, 1, t);
			roi.setPosition( index );
		}
		else{
			roi.setPosition(t);
		}
	}*/
	
	//Roi positioning from ij.plugin.OverlayCommands
	private void setRoiFrame(Roi roi, int t){
        if (imp.isHyperStack()||imp.isComposite()) {
            roi.setPosition(0, 0, t);
        }
        else{
            roi.setPosition(t);
        }
	}
	
	/** Detects filopodia in the image.
	 * 
	 * @param prev	true to run a preview, mapping only the currently displayed frame, false to run on all frames and continue to filtering and tracking
	 * @param filoProcessor	the <code>FilopodyanProcessor</code> to use to generate the binary mask
	 * @see LoGProcessor
	 * @see ALTProcessor
	 * @see ProcessProcessor
	 * @see Tipper
	*/	
	public void filopodia(boolean prev, FilopodyanProcessor filoProcessor){
	try{
		bgui.setLabel("mapping processes<br>"+title);
		if(bgui.verbose){bgui.log.print(title, "Mapping processes in "+title);}
		if(!prev){setImageVisible(false);}

		//***********************************
		//*	  tStart and tEnd are 1-based   *
		//***********************************
		tStart = 1;
		tEnd = T;
		if(prev){
			tStart = imp.getFrame();
			tEnd = tStart;
		}
		Prefs.blackBackground = true;
		ImageCalculator ic = new ImageCalculator();
	
		Roi userRoi = null;
		if(!batch){
			imp.setOverlay(new Overlay());
			userRoi = imp.getRoi();
			if(userRoi!=null&&userRoi.getType()>3){userRoi=null;}	//ignore if not an area
			IJ.run(imp, "Select None", "");
		}
		
		if(bgui.adaptive){
			bgui.setLabel("adaptive thresholding<br>"+title);
			if(bgui.verbose){bgui.log.print(title, "Running adaptive thresholding");}
			map = filoProcessor.process(imp,bgui.mapC,tStart, tEnd, bgui.sigma, bgui.threshold, bgui.verbose);
		}
		else{
			bgui.setLabel("Laplacian of Gaussian<br>"+title);
			if(bgui.verbose){bgui.log.print(title, "Running Laplacian of Gaussian");}
			map = filoProcessor.process(imp,bgui.mapC,tStart, tEnd, bgui.sigma, bgui.threshold, bgui.verbose);
		} 
		
		if(userRoi!=null){
			if(bgui.verbose){IJ.log("Analysing user-defined ROI");}
			map.setRoi(userRoi);
			IJ.run(map, "Clear Outside", "stack");
			IJ.run(map, "Select None", "");
		}
		
		if(bgui.join){
			map = new ProcessProcessor().join(map);
		}
		
		frameBackgroundRoiArray = new Roi[tEnd+1];
		boundaryBackgroundRoiArray = new Roi[tEnd+1];
		for(int t=1;t<=map.getNFrames();t++){
			map.setPosition(1, 1, t);
			IJ.run(map, "Create Selection", "");
			if(map.getStatistics().mean<=0.0001){
				IJ.run(map, "Make Inverse", "");
			}
			if(map.getRoi()!=null){
				Roi signalRoi = new ShapeRoi( map.getRoi() );
				IJ.run(map, "Make Inverse", "");
				if(map.getRoi()!=null){ //check again after making inverse - NullPointerException reported by user, could not replicate
					ShapeRoi inverseRoi = new ShapeRoi( map.getRoi() );
					frameBackgroundRoiArray[t] = inverseRoi;
					
					ShapeRoi enlarged = new ShapeRoi( RoiEnlargerHandler.enlarge(signalRoi, 20) );
					boundaryBackgroundRoiArray[t] = enlarged.xor((ShapeRoi) signalRoi);

					setRoiFrame(frameBackgroundRoiArray[t], t);
					setRoiFrame(boundaryBackgroundRoiArray[t], t);
					
					map.killRoi();
				}
			}
		}
		
		if(!prev)maxAreaOnly(map,tStart,tEnd);
		
		body = dup.run(map, 1, 1, 1, 1, tStart, tEnd);
		body.setTitle("body");

		bgui.setLabel("segmenting<br>"+title);
		if(bgui.verbose){bgui.log.print(title, "Running binary segmentation");}
		
		//use ByteProcessors directly - thread safe
		for(int i=1;i<=body.getNFrames();i++){
			body.setPosition(1,1,i);
			ByteProcessor bite = (ByteProcessor)body.getProcessor();
			for(int e=0;e<bgui.eds;e++){
				bite.erode(1,0);
			}
			for(int d=0;d<bgui.eds;d++){
				bite.dilate(1,0);
			}
			body.setProcessor(bite);
		}
		IJ.run(body, "Fill Holes", "stack");
		maxAreaOnly(body,tStart,tEnd);
		
		IJ.run(body, "Median...", "radius="+bgui.eds+" stack");	//smooth sharp corners from erode/dilate
		
		proj = ic.run("Subtract create stack", map, body);
		
		IJ.run(proj, "Median...", "radius=1 stack");	//smooth sharp corners from erode/dilate
		
		proj.setTitle("proj");
		IJ.run(proj, "Fill Holes", "stack");
		if(prev){
			IJ.run(body, "Select None", "");
			Overlay prevol = new Overlay();
			IJ.run(body, "Create Selection", "");
			if(body.getRoi()!=null&&body.getStatistics().mean==0){
				IJ.run(body, "Make Inverse", "");
			}
			if(body.getRoi()!=null){
				Roi bodyRoi = body.getRoi();
				bodyRoi.setStrokeColor(Color.RED);
				setRoiFrame(bodyRoi, tStart);
				prevol.add(bodyRoi);
			}
			IJ.run(proj, "Create Selection", "");
			if(proj.getRoi()!=null){
				Roi projRoi = proj.getRoi();
				projRoi.setStrokeColor(Color.PINK);
				setRoiFrame(projRoi, tStart);
				prevol.add(projRoi);
			}
			imp.setOverlay(prevol);
			return;
		}
		
		filo = new ArrayList<ArrayList<FiloPod>>();
		backRoi = new ArrayList<Roi>();
		localBackgroundRois = new ArrayList<Roi>();
		ol = new Overlay();
		bodyRT = new ResultsTable();
		bodyRT.setPrecision(3);
		bodyRT.showRowNumbers(false);
		bodyRoiArr = new ShapeRoi[tEnd];
		bodyMean = new double[tEnd];
		ind = -1;
		for(int t=tStart;t<=tEnd;t++){
			bgui.setLabel("mapping processes T"+t+"<br>"+title);
			if(bgui.verbose){bgui.log.print(title, "Analysing objects at T"+t);}
			imp.setPosition(1,1,t);
			ArrayList<FiloPod> timeFilo = new ArrayList<FiloPod>();
			proj.setPosition(1,1,t);
			IJ.run(proj, "Create Selection", "");
			if(proj.getRoi()==null){continue;}
			if(proj.getStatistics().mean==0){
				IJ.run(proj, "Make Inverse", "");
			}
			if(proj.getRoi()==null){
				bgui.log.print(title, title+" - no projections found at T"+t);
				filo.add(timeFilo);
				bodyRT.setValue("T",t,t);
				continue;
			}
			if(bgui.verbose){bgui.log.print(title, "Got projections at T"+t+" for "+title);}
			ShapeRoi projRoi = new ShapeRoi(proj.getRoi());
			
			body.setPosition(bgui.mapC,1,t);
			IJ.run(body, "Create Selection", "");
			if(body.getRoi()==null){continue;}
			if(body.getStatistics().mean==0){
				IJ.run(body, "Make Inverse", "");
			}
			if(body.getRoi()==null){
				bgui.log.print(title, title+" - no growth cone body found at T"+t);
				while(bodyRT.getCounter()<t){
					bodyRT.setValue("T", bodyRT.getCounter(), Double.NaN);
				}
				bodyRT.setValue("T",t,t);
				continue;
			}
			if(bgui.verbose){bgui.log.print(title, "Got growth cone body");}
			ShapeRoi bodyRoi = new ShapeRoi(body.getRoi());
			Roi[] bs = bodyRoi.getRois();
			if(bs.length>1){	//avoid composite Rois
				double maxL = -1d;
				for(Roi r : bs){
					if(r.getLength()>maxL){
						bodyRoi = new ShapeRoi(r);
						maxL = r.getLength();
					}
				}
			}
			setRoiFrame(bodyRoi, t);
			bodyRoi.setStrokeColor(Color.MAGENTA);
			bodyRoiArr[t-1] = bodyRoi;
			
			Rectangle rr = bodyRoi.getBounds();
			imp.setPosition(bgui.measureC,1,t);
			imp.setRoi(bodyRoi);
			ImageStatistics bodyStats = imp.getStatistics();
			imp.setRoi(frameBackgroundRoiArray[t]);
			ImageStatistics frameBackgroundStats = imp.getStatistics();
			imp.setRoi(boundaryBackgroundRoiArray[t]);
			ImageStatistics boundaryBackgroundStats = imp.getStatistics();
			imp.killRoi();
			
			int tablei = t-1;
			if(tablei>bodyRT.getCounter()){throw new IndexOutOfBoundsException("bodyTable row out of bounds : "+tablei+"/"+bodyRT.getCounter());}
			bodyRT.setValue("T",tablei,t);
			bodyRT.setValue("X",tablei,(rr.x+(rr.width/2))*pixelW);
			bodyRT.setValue("Y",tablei,(rr.y+(rr.height/2))*pixelW);
			bodyRT.setValue("Mean",tablei,bodyStats.mean);
			bodyMean[t-1] = bodyStats.mean;
			bodyRT.setValue("StdDev",tablei,bodyStats.stdDev);
			bodyRT.setValue("Frame Background",tablei,frameBackgroundStats.mean);
			bodyRT.setValue("Boundary Background",tablei,boundaryBackgroundStats.mean);

			PointRoi bodyCentroid = new PointRoi(rr.x+(rr.width/2),rr.y+(rr.height/2));
			
			setRoiFrame(bodyCentroid, t);
			bodyCentroid.setStrokeColor(Color.MAGENTA);
			ol.add(bodyCentroid);
			
			Roi[] split = projRoi.getRois();
			
			imp.setPosition(bgui.measureC,1,t);
			if(bgui.verbose){bgui.log.print(title, "Processing "+split.length+" objects...");}
			Tipper tipper = new Tipper();
			for(int f=0;f<split.length;f++){
				if(bgui.verbose){bgui.log.print(title, "Object "+f);}
				if(onEdge(split[f])){continue;}
				ind++;
				imp.setRoi(split[f]);
				double area = imp.getStatistics().area;
				double projMean = imp.getStatistics().mean;
				ShapeRoi grow = new ShapeRoi(RoiEnlargerHandler.enlarge(split[f],3));
				ShapeRoi baseRoi = grow.and(bodyRoi);
				setRoiFrame(baseRoi, t);
				baseRoi.setStrokeColor(Color.YELLOW);
				imp.setRoi(baseRoi);
				double baseMean = imp.getStatistics().mean;		
				Rectangle rect = baseRoi.getBounds();
				int baseX = rect.x+(rect.width/2);
				int baseY = rect.y+(rect.height/2);
				if(bgui.verbose){bgui.log.print(title, "Finding process tip");}
				Roi tipRoi = tipper.findTip(imp,split[f],new Point(baseX,baseY),bgui.measureC,t,bgui.fit);
				imp.setRoi(tipRoi);
				double tipMean = imp.getStatistics().mean;
				ImagePlus tipMeasure = dup.run(imp,bgui.measureC,bgui.measureC,1,1,t,t);
				IJ.setAutoThreshold(tipMeasure, "Otsu dark stack");
				IJ.run(tipMeasure, "Create Selection", "");
				double tipThMean = tipMeasure.getStatistics().mean;	//mean of Otsu thresholded values in tip ROI			
				tipMeasure.close();
				setRoiFrame(tipRoi, t);
				tipRoi.setStrokeColor(Color.GREEN);
	
				Roi processRoi = (Roi)new ShapeRoi(split[f]).or(new ShapeRoi(tipRoi));
				Roi clone = (Roi)processRoi.clone();
				if(processRoi.getType()==Roi.COMPOSITE){
					int n = 0;
					while(processRoi.getType()==Roi.COMPOSITE&&n<15){
						if(bgui.verbose){bgui.log.print(title, "composite join "+n+" Roi length = "+processRoi.getLength());}
						if(processRoi.getLength()<1){processRoi = clone; break;}
						processRoi = RoiEnlargerHandler.enlarge(processRoi,n);
						if(processRoi==null){
							processRoi = clone;
							break;
						}
						if(processRoi.getLength()<1){processRoi = clone; break;}
						processRoi = RoiEnlargerHandler.enlarge(processRoi,-n);
						if(processRoi==null){
							processRoi = clone;
							break;
						}
						n++;
						if(processRoi.getLength()<1){processRoi = clone; break;}
					}
				}
				setRoiFrame(processRoi, t);
				processRoi.setStrokeColor(Color.CYAN);
				Filopart fp = new Filopart(processRoi,(Roi)baseRoi,tipRoi,pixelW,t,ind,area,baseMean,projMean,tipMean,tipThMean,bgui.sigma);
				timeFilo.add( fp );
			}
			filo.add(timeFilo);
		}
		IJ.run(imp, "Select None", "");	
		LinearAssigner LA1 = new OneStepAssigner(bgui.verbose);
		filo = LA1.assign(filo, imp);
		doOverlay();
		
		imp.setOverlay(ol);
		IJ.run(imp, "Select None", "");
		
		if(prev)return;
		
		setImageVisible(true);
		
		calculateDCMs();
		
		FiloFilter cf = new FiloFilter(filo,ind,this);
		if(batch){
			cf.batchFilter((BatchFilopodyan)bgui);
		}
		else{cf.run();}
		
		if(body!=null){body.changes = false; body.close();}
		if(proj!=null){proj.changes = false; proj.close();}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	finally{setImageVisible(true);}
	}
	
	
	private void calculateDCMs(){
	try{
		for(int i=0;i<ind;i++){
			Point2d tipLast = new Point2d();
			Point2d baseLast = new Point2d();
			int lastT = -100;
			time:
			for(int t=tStart;t<=tEnd;t++){
				int tzb = t-1;	//zero-based value for ArrayList indexes
				for(int f=0;f<filo.get(tzb).size();f++){
					FiloPod part = filo.get(tzb).get(f);
					if(part.getIndex()==i){
						if(lastT==tzb-1){
							Vector baseToTip = new Vector(part.getBaseCoord(), part.getTipCoord());
							Vector tipMove = new Vector(tipLast, part.getTipCoord());
							Vector baseMove = new Vector(baseLast, part.getBaseCoord());
							part.setDctm(tipMove.getRelativeMagnitude(baseToTip));
							part.setDcbm(baseMove.getRelativeMagnitude(baseToTip));
						}
						tipLast = part.getTipCoord();
						baseLast = part.getBaseCoord();
						lastT = tzb;
						continue time;
					}
				}
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Called by <code>FiloFilter</code> when filters have been applied. Takes the filtered <code>Filopart</code> list and creates a <code>TrackEditor</code>.
	 * 
	 * @param backPass	The <code>Filopart</code> Collection for use in <code>TrackEditor</code>. This is a List of timepoints each having a List of <code>FiloPart</code>s.
	 * @see FiloFilter
	 * @see TrackEditor
	 * @see Filopart
	 */
	public void filtered(ArrayList<ArrayList<FiloPod>> backPass){
		filo = backPass;
		LinearAssigner LA2 = new OneStepAssigner(bgui.verbose);
		filo = LA2.assign(filo, imp);
		calculateDCMs();
		TrackEditor trackEditor = new TrackEditor(filo,this);
		if(batch){
			trackEditor.sequentialise();
			filo = trackEditor.filo;
			output(filo);
		}
		else{
			trackEditor.run();		//allows manual editing of tracks, then calls back trackEdited(ArrayList<ArrayList<Filopart>> backPass)
		}
	}
	
	/** Called by <code>TrackEditor</code> to output edited Tracks.
	 * 
	 * @param backPass	The <code>Filopart</code> Collection for output. This is a List of timepoints each having a List of <code>FiloPart</code>s.
	 * @see Filopart
	 * @see TrackEditor
	 */
	public void trackEdited(ArrayList<ArrayList<FiloPod>> backPass){
		filo = backPass;
		output(filo);
	}
	
	/** Updates the <code>Overlay</code> and displays it on the current image.
	 * 
	 *  @param f	The <code>Filopart</code> Collection to be shown in the Overlay. This is a List of timepoints each having a List of FiloParts.
	 *  @see Filopart
	 *  @see Filopodyan_#doOverlay()
	 */
	public void update(ArrayList<ArrayList<FiloPod>> f){
		if(f.size()>=0){
			filo = f;
			doOverlay();
			imp.setOverlay(ol);
		}
	}
	
	/** Creates the <code>Overlay</code> from the current <code>FiloPod</code>s
	 * 
	 * @see FiloPod, Filopart
	 */
	public void doOverlay(){
	try{
		if(batch){return;}
		ol = new Overlay();
		bgui.setLabel("making overlay<br>"+title);
		if(bgui.verbose){bgui.log.print(title, "Making Overlay for "+title);}
		firstTrackIndex = Integer.MAX_VALUE;
		for(int t=0;t<filo.size();t++){	//make overlay
			if(bodyRoiArr[t]==null||filo==null||filo.size()==0){continue;}
			setRoiFrame(bodyRoiArr[t], t+1);
			ol.add(bodyRoiArr[t]);
			for(int a=0;a<filo.get(t).size();a++){
				FiloPod part = filo.get(t).get(a);
				if(part.getIndex()==-1){continue;}
				firstTrackIndex = (int)Math.min(firstTrackIndex, part.getIndex());
				String str = String.valueOf(part.getIndex());
				TextRoi label = new TextRoi( part.getBaseCoord().x/pixelW, part.getBaseCoord().y/pixelW, str, labelFont );

				setRoiFrame(part.getRoi(), t+1);
				setRoiFrame(part.getBase(), t+1);
				setRoiFrame(part.getTip(), t+1);
				setRoiFrame(label, t+1);
				ol.add(part.getRoi());
				ol.add(part.getBase());
				ol.add(part.getTip());

				label.setStrokeColor(Color.CYAN);
				ol.add(label);
			}
		}
		for(int b=0;b<backRoi.size();b++){
			ol.add(backRoi.get(b));
		}
		if(bgui.showBackground){
			for(Roi roi : frameBackgroundRoiArray){
				if(roi==null)continue;
				roi.setFillColor( frameBackgroundColor  );
				ol.add(roi);
			}
			for(Roi roi : boundaryBackgroundRoiArray){
				if(roi==null)continue;
				roi.setFillColor( boundaryBackgroundColor );
				ol.add(roi);
			}
			for(Roi roi : localBackgroundRois){
				if(roi==null)continue;
				roi.setFillColor( localBackgroundColor );
				ol.add(roi);
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Measures all filopodia and shows them in <code>ResultsTable</code>s and the image <code>Overlay</code>.
	 * 
	 * @param backPass	The <code>FiloPod</code> Collection for output. This is a List of timepoints each having a List of <code>FiloPod</code>s.
	 * @see Filopart
	 */
	public void output(final ArrayList<ArrayList<FiloPod>> backPass){
		try{
			if(!batch){bgui.workFrame.setVisible(true);}
			SwingWorker<Object, Void> worker = new SwingWorker<Object, Void>(){
				public Object doInBackground(){
					
					setImageVisible(false);
					bgui.setLabel("analysing objects<br>"+title);
					if(bgui.verbose){bgui.log.print(title, "Analysing objects...");}
					filo = backPass;
					if(bgui.boundaryAnalysis){
						new BoundaryAnalyser(bgui,imp).run(filo,bodyRoiArr);
					}

					//doOverlay();

					filoRT = new ResultsTable();
					filoRT.showRowNumbers(false);
					filoRT.setPrecision(3);

					coordRT = new ResultsTable();

					for(int t=0;t<T+Math.abs(bgui.backFrames);t++){
						filoRT.setValue("dT",t,bgui.backFrames+t);
						//coordRT.setValue("T ("+firstTrackIndex+")",t,bgui.backFrames+t);
						coordRT.setValue("dT",t,bgui.backFrames+t);
					}

					for(int i=0;i<ind;i++){
						bgui.setLabel("analysing track "+i+"<br>"+title);
						if(bgui.verbose){bgui.log.print(title, "Measuring track "+i);}
						double[] baseMeanArr = new double[T];
						double[] projMeanArr = new double[T];
						double[] tipMeanArr = new double[T];
						double[] tipThMeanArr = new double[T];

						double[] localTipBackgroundArr = new double[T];
						double[] localBaseBackgroundArr = new double[T];

						double[] lengthArr = new double[T];
						double[] dlArr = new double[T];
						double[] dctmArr = new double[T];
						double[] dcbmArr = new double[T];
						Point2d[] baseCoordArr = new Point2d[T];
						Point2d[] tipCoordArr = new Point2d[T];
						int[] dtArr = new int[T];
						int start = Integer.MAX_VALUE;
						for(int ti=tStart;ti<=tEnd;ti++){
							try{
								int t = ti-1;
								for(int f=0;f<filo.get(t).size();f++){
									FiloPod part = filo.get(t).get(f);
									if(part.getIndex()==i){
										if(start==Integer.MAX_VALUE){
											start = t;
											dtArr[t] = 0;
										}
										else{dtArr[t] = t-start;}
										baseMeanArr[t] = part.getBaseMean();
										projMeanArr[t] = part.getProjMean();
										tipMeanArr[t] = part.getTipMean();
										tipThMeanArr[t] = part.getTipThMean();

										imp.setPosition(bgui.measureC, 1, t);
										Roi tipBackgroundRoi = new ShapeRoi(RoiEnlargerHandler.enlarge( part.getTip(), 5 )).xor( new ShapeRoi(part.getTip()) ).not( new ShapeRoi(part.getRoi()) ).not( new ShapeRoi(bodyRoiArr[t] ));
										if(tipBackgroundRoi!=null){
											imp.setRoi(tipBackgroundRoi);
											localTipBackgroundArr[t] = imp.getStatistics().mean;
											setRoiFrame(tipBackgroundRoi, t+1);
											localBackgroundRois.add(tipBackgroundRoi);
										}
										Roi baseBackgroundRoi = null;
										try{
											baseBackgroundRoi = new ShapeRoi(RoiEnlargerHandler.enlarge( part.getBase(), 5 ))
																											.xor( new ShapeRoi(part.getBase()) )
																											.not( new ShapeRoi(part.getRoi()) )
																											.not( new ShapeRoi(bodyRoiArr[t] ));
										}catch(IllegalArgumentException iae){}	//ignore exception caused by zero area ShapeRoi, baseBackgroundRoi remains null
										if(baseBackgroundRoi!=null){
											imp.setRoi(baseBackgroundRoi);
											localBaseBackgroundArr[t] = imp.getStatistics().mean;
											setRoiFrame(baseBackgroundRoi, t);
											localBackgroundRois.add(baseBackgroundRoi);
										}

										lengthArr[t] = part.getLength();
										dctmArr[t] = part.getDctm();
										dcbmArr[t] = part.getDcbm();
										baseCoordArr[t] = part.getBaseCoord();
										tipCoordArr[t] = part.getTipCoord();
										if(t>0){ dlArr[t] = lengthArr[t]-lengthArr[t-1]; }
										Roi backBase = new OvalRoi((part.getBaseCoord().x/pixelW)-4.5,(part.getBaseCoord().y/pixelW)-4.5,9,9);	//use the first real base coordinate for the first back projection
										int backX = -1;
										int backY = -1;
										for(int back=-1;back>=bgui.backFrames;back--){
											if(t+back>=0 && baseMeanArr[t+back]==0){
												imp.setPosition(bgui.measureC,1,t+back+1);
												imp.setRoi(bodyRoiArr[t+back]);
												IJ.run(imp, "Interpolate", "interval=1");
												ShapeRoi bodyShape = new ShapeRoi(imp.getRoi());			
												Polygon bodyPolygon = bodyShape.getPolygon();

												//	backBase = new OvalRoi((part.baseCoord.x/pixelW)-4.5,(part.baseCoord.y/pixelW)-4.5,9,9);	//reset to first existing base each time
												Rectangle previousRect = backBase.getBounds();
												backBase = new OvalRoi(previousRect.x,previousRect.y,9,9);		//use the previous base coordinates for a priori base position

												double minBodyD = Double.POSITIVE_INFINITY;
												int minBodyI = -1;
												Rectangle baseRect = backBase.getBounds();
												double baseX = baseRect.x + (baseRect.width/2);
												double baseY = baseRect.y + (baseRect.height/2);
												for(int p=0;p<bodyPolygon.npoints;p++){
													double dist = ((bodyPolygon.xpoints[p]-baseX)*(bodyPolygon.xpoints[p]-baseX)) + ((bodyPolygon.ypoints[p]-baseY)*(bodyPolygon.ypoints[p]-baseY));
													if(dist<minBodyD){
														minBodyD = dist;
														minBodyI = p;
													}
												}
												backX = Math.round( bodyPolygon.xpoints[minBodyI]-4);
												backY = Math.round(bodyPolygon.ypoints[minBodyI]-4);
												backBase.setLocation(backX,backY);
												backBase = new ShapeRoi(backBase).and(bodyShape);
												imp.setRoi(backBase);
												ImageStatistics backStats = imp.getStatistics();
												baseMeanArr[t+back] = backStats.mean;
												baseCoordArr[t+back] = new Point2d(backX*pixelW,backY*pixelW);
												dtArr[t+back] = back;

												baseBackgroundRoi = new ShapeRoi(RoiEnlargerHandler.enlarge( backBase, 5 )).xor( (ShapeRoi)backBase ).not( new ShapeRoi(bodyRoiArr[t+back] ));
												if(baseBackgroundRoi!=null){
													imp.setRoi(baseBackgroundRoi);
													localBaseBackgroundArr[t+back] = imp.getStatistics().mean;

													setRoiFrame(baseBackgroundRoi, t+back+1);
													localBackgroundRois.add(baseBackgroundRoi);
												}

												setRoiFrame(backBase, t+back+1);
												backBase.setStrokeColor(Color.ORANGE);
												backRoi.add(backBase);
												TextRoi backLabel = new TextRoi(backX,backY,""+i,labelFont);
												setRoiFrame(backLabel, t+back+1);
												backLabel.setStrokeColor(Color.ORANGE);
												backRoi.add(backLabel);
											}
											else break;
										}
									}
								}
								IJ.run(imp, "Select None", "");
							}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
						}
						if(!bgui.filoTable&&!bgui.coordTable){continue;}
						try{
							int rtrow = -1;
							int ctrow = -1;
							for(int ti=tStart;ti<=tEnd;ti++){
								int t = ti - 1;
								rtrow = t-start-bgui.backFrames;
								ctrow = rtrow;
								if(baseCoordArr[t]!=null){	//if it has a base or back-projected base at this t
									filoRT.setValue("T ("+i+")",rtrow,ti);
									filoRT.setValue("dT ("+i+")",rtrow,filoRT.getValue("dT",rtrow));	//duplicate column, user request
									filoRT.setValue("Base Mean ("+i+")",rtrow,baseMeanArr[t]);
									filoRT.setValue("Base Local Background ("+i+")",rtrow,localBaseBackgroundArr[t]);
									filoRT.setValue("Body Mean ("+i+")",rtrow,bodyMean[t]);
									coordRT.setValue("T ("+i+")",ctrow,ti);
									coordRT.setValue("Base X ("+i+")",ctrow,baseCoordArr[t].x);
									coordRT.setValue("Base Y ("+i+")",ctrow,baseCoordArr[t].y);
								}

								if(tipCoordArr[t]!=null){	//if it has a tip at this t
									filoRT.setValue("Proj Mean ("+i+")",rtrow,projMeanArr[t]);
									filoRT.setValue("Tip Mean ("+i+")",rtrow,tipMeanArr[t]);
									filoRT.setValue("Tip Th Mean ("+i+")",rtrow,tipThMeanArr[t]);
									filoRT.setValue("Tip Local Background ("+i+")",rtrow,localTipBackgroundArr[t]);
									filoRT.setValue("Length ("+i+")",rtrow,lengthArr[t]);
									filoRT.setValue("dL ("+i+")",rtrow,dlArr[t]);

									coordRT.setValue("Tip X ("+i+")",ctrow,tipCoordArr[t].x);
									coordRT.setValue("Tip Y ("+i+")",ctrow,tipCoordArr[t].y);

									Point2D.Double body = new Point2D.Double(bodyRT.getValue("X",t),bodyRT.getValue("Y",t));
									Point2D.Double base = new Point2D.Double(baseCoordArr[t].x,baseCoordArr[t].y);
									Point2D.Double tip = new Point2D.Double(tipCoordArr[t].x,tipCoordArr[t].y);
									Vector bodyToBase = new Vector(body,base);
									Vector baseToTip = new Vector(base,tip);

									coordRT.setValue("||[body,base]|| ("+i+")",ctrow,bodyToBase.getMagnitude());
									coordRT.setValue("||[base,tip]||  ("+i+")",ctrow,baseToTip.getMagnitude());
									coordRT.setValue("||[base,tip]|| : [body,base] ("+i+")",ctrow,baseToTip.getRelativeMagnitude(bodyToBase));	//magnitude of baseToTip projected onto bodyToBase
									coordRT.setValue("[base,tip] \u22C5 [body,base] ("+i+")",ctrow,bodyToBase.dotProduct(baseToTip));

									filoRT.setValue("DCTM ("+i+")",rtrow,Double.NaN);	//make sure column is always added
									filoRT.setValue("DCBM ("+i+")",rtrow,Double.NaN);
									if(t>=tStart&&ctrow>0){
										coordRT.setValue("\u0394||[base,tip]|| : [body,base] ("+i+")",ctrow,coordRT.getValue("||[base,tip]|| : [body,base] ("+i+")",ctrow)-coordRT.getValue("||[base,tip]|| : [body,base] ("+i+")",ctrow-1));
										if(tipCoordArr[t]==null){continue;}	//index wasn't assigned for this frame
										coordRT.setValue("||[tip(t-1),tip(t)]|| : [base,tip] ("+i+")",ctrow,dctmArr[t]);	//DCTM - tip movement from t-1 to t projected onto filopodium vector
										coordRT.setValue("||[base(t-1),base(t)]|| : [base,tip] ("+i+")",ctrow,dcbmArr[t]);	//DCBM - base movement from t-1 to t projected onto filopodium vector
										filoRT.setValue("DCTM ("+i+")",rtrow,dctmArr[t]);
										filoRT.setValue("DCBM ("+i+")",rtrow,dcbmArr[t]);
									}
								}
							}
						}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
						//doOverlay();
					}
					doOverlay();
					bgui.setLabel("outputting results<br>"+title);
					if(bgui.filoTable){
						//set zeros representing absence of an object to NaN before showing the table
						String[] head = filoRT.getColumnHeadings().split("\t");
						for(int c=0;c<head.length;c++){
							for(int r=0;r<filoRT.getCounter();r++){
								double value = filoRT.getValue(head[c],r);
								if(value==0){
									if(filoRT.getValue("dT",r)==0&&(head[c].startsWith("dT")||head[c].startsWith("T ("))){continue;}
									if(head[c].startsWith("DC")&&filoRT.getValue("dT",r)>0){
										int track = Integer.valueOf(head[c].substring(head[c].indexOf("(")+1,head[c].indexOf(")")));
										if(!Double.isNaN(filoRT.getValue("Body Mean ("+track+")",r))){
											continue;
										}
									}
									if(head[c].startsWith("T (")){
										int track = Integer.valueOf( head[c].substring( head[c].indexOf("(")+1, head[c].indexOf(")") ) );
										if(!Double.isNaN(filoRT.getValue("Body Mean ("+track+")",r))&&(filoRT.getValue("Body Mean ("+track+")",r)!=0)){
											continue;
										}
									}
									if(head[c].startsWith("dL (")){
										int track = Integer.valueOf(head[c].substring(head[c].indexOf("(")+1,head[c].indexOf(")")));
										if(!Double.isNaN(filoRT.getValue("Length ("+track+")",r))&&(filoRT.getValue("Length ("+track+")",r)!=0)){
											continue;
										}
									}
									filoRT.setValue(head[c],r,Double.NaN);
								}
							}
						}
						filoRT.show(sanTitle+" Filopodia");
					}

					if(bgui.coordTable){

						//set zeros representing absence of an object to NaN
						String[] head = coordRT.getColumnHeadings().split("\t");
						for(int c=0;c<head.length;c++){
							try{
								if(head[c].matches(" ")){continue;}
								for(int r=0;r<coordRT.getCounter();r++){
									double value = coordRT.getValue(head[c],r);
									if(value==0){
										if(head[c].startsWith("T (")){
											int track = Integer.valueOf( head[c].substring( head[c].indexOf("(")+1, head[c].indexOf(")") ) );
											if(!Double.isNaN(coordRT.getValue("Base X ("+track+")",r))&&coordRT.getValue("Base X ("+track+")",r)!=0){
												continue;
											}
										}
										if(!head[c].matches("dT")){
											coordRT.setValue(head[c],r,Double.NaN);
										}
									}
								}
							}catch(Exception e){IJ.log(e.toString()+"\nHeading: "+head[c]);}
						}
						coordRT.show(sanTitle+" Coordinates");
					}

					if(bgui.bodyTable){bodyRT.show(sanTitle+" Bodies");}

					imp.setOverlay(ol);
					imp.setC(bgui.mapC);
					if(!batch){bgui.workFrame.dispose();}
					setImageVisible(true);
					if(bgui.verbose){
						bgui.log.print(title, "Filopodyan finished");
					}

					if(batch){
						try{
							String tablePath = ((BatchFilopodyan)bgui).path+File.separator+"Filopodyan_tables"+File.separator;
							File dirFile = new File(tablePath);
							if(!dirFile.exists()){ dirFile.mkdirs(); }
							if(bgui.filoTable){filoRT.saveAs(tablePath+sanTitle+"_Filopodia.csv");}
							if(bgui.bodyTable){bodyRT.saveAs(tablePath+sanTitle+"_Bodies.csv");}
							if(bgui.coordTable){coordRT.saveAs(tablePath+sanTitle+"_Coordinates.csv");}
							if(bgui.filoTable||bgui.bodyTable||bgui.coordTable){bgui.log.print(title, title+" tables saved in "+tablePath);}
						}catch(IOException ioe){IJ.log(ioe.toString());}
						((BatchFilopodyan)bgui).count.getAndDecrement();
					}
					if(bgui.log!=null){
						bgui.log.print(title, "");	//add empty line
						bgui.log.setVisible(true);
					}
					return null;
				}
			};
			worker.execute();
		}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}

	/** Checks if an image has been set for analysis.
	 * 
	 * @return	true if an image has been set and is visible, false otherwise
	 * @see Filopodyan_#setImp(ImagePlus imp)
	 */
	public boolean gotImage(){
		boolean ans = false;
	try{
		ans = imp.isVisible();
		if(!ans){
			IJ.error("Where did "+title+" go?");
			bgui.dispose();
		}
		return ans;
	}catch(Exception e){IJ.log(e.toString()+"\n~~##~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return ans;
	}
	

	@Override
	/** Run as <code>org.scijava.command.Command</code>. Create and show a <code>FilopodyanGui</code> if an image is open or a <code>BatchFilopodyan</code> if there are no images.
	 */
	public void run(){
	try{
		if(WindowManager.getImageCount()==0){
			bgui = new BatchFilopodyan();
			((BatchFilopodyan)bgui).createDialog();
			bgui.toFront();
			return;
		}
		imp = WindowManager.getCurrentImage();
		title = imp.getTitle();
		
		sanTitle = title.replaceAll("[^a-zA-Z0-9_]","_");
		W = imp.getWidth();
		H = imp.getHeight();
		C = imp.getNChannels();
		Z = imp.getNSlices();
		T = imp.getNFrames();
		Calibration cal = imp.getCalibration();
		pixelW = cal.pixelWidth;
		unit = imp.getCalibration().getUnit();
		if(pixelW==1){
			String str = (String)JOptionPane.showInputDialog(null,"Pixel size ("+unit+")",title,JOptionPane.PLAIN_MESSAGE,null,null, ""+defaultPixelW);
			if(str!=null){
				try{
					pixelW = Double.valueOf(str);
				}catch(NumberFormatException nfe){IJ.error(str+" is not a number");return;}
				cal.pixelWidth = pixelW;
				cal.pixelHeight = pixelW;
				imp.setCalibration(cal);
				imp.getWindow().repaint();
			}
		}
		cal.getTimeUnit();
		
		if(unit.matches("[Mm]icrons?")){unit="\u00B5m";}
	
		bgui = new FilopodyanGui(this,imp);
		
		imp.getWindow().toFront();
		bgui.toFront();
		
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Used for testing only.
	 * 
	 * 	@param arg	not used
	 * */
	public static void main(String[] arg){
		
		/*JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Filopodyan test image...");
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
	    if(fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
	    	String path = fc.getSelectedFile().getAbsolutePath();
	    	ImagePlus image = new ImagePlus(path);
			image.show();
	    }*/
		
		ImageJ.main(arg);
		//ImagePlus img = new ImagePlus("E:\\Vasja\\t1ol_bug_20180129\\NeonENA_GC4_huang4-01_ed4_small.tif");
		//final ImagePlus image = HyperStackConverter.toHyperStack(img, 2, 1, 8);
		ImagePlus img = new ImagePlus("E:\\Jenny Gallop\\fly_figure\\MAX_2016-0330-fascinGFP enGal4 UAS-cd8mCherry.lif - Series006-6T.tif");
		final ImagePlus image = HyperStackConverter.toHyperStack(img, 2, 1, 6);
		image.setDisplayMode(IJ.COLOR);
		image.show();
	    
		new Filopodyan_().run();
	}
	
}

