package uk.ac.cam.gurdon;
import java.awt.Color;
import java.awt.Font;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import ij.IJ;
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
import ij.plugin.ImageCalculator;
import ij.plugin.PlugIn;
import ij.process.ByteProcessor;
import ij.process.ImageStatistics;

public class Filopodyan_ implements PlugIn{
public ImagePlus imp;
private ImagePlus map,body,proj;
public FilopodyanGui bgui;
public int W,H,C,Z,T,ind,tStart,tEnd,firstTrackIndex;
public String title,sanTitle;
private String unit;
private double pixelW;
private ResultsTable filoRT, bodyRT, coordRT;
private Duplicator dup = new Duplicator();
private ArrayList<ArrayList<Filopart>> filo;
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
private static final boolean INDEX1D = false;	//Overlay Roi slice index behaviour is inconsistent between versions, this sets the Roi.setPosition method to use

	public void setImageVisible(final boolean v){
	try{
		if(batch){return;}
		imp.getWindow().setVisible(v);
		if(v)IJ.run(imp, "Enhance Contrast...", "saturated=0.4");
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void setImp(ImagePlus image){
		imp = image;
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
			target.setRoi(split[maxI]);
			IJ.setBackgroundColor(0, 0, 0);
			IJ.run(target, "Clear Outside", "slice");
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
	
	public void filopodia(boolean prev){
	try{
		bgui.setLabel("mapping processes<br>"+title);
		if(bgui.verbose){bgui.log.print(title, "Mapping processes in "+title);}
		if(!prev){setImageVisible(false);}
	/*
	***********************************
	*	tStart and tEnd are 1-based
	***********************************
	*/	
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
		
		FilopodyanProcessor processor = new FilopodyanProcessor();
		if(bgui.adaptive){
			bgui.setLabel("adaptive thresholding<br>"+title);
			if(bgui.verbose){bgui.log.print(title, "Running adaptive thresholding");}
			map = processor.ALT(imp,bgui.mapC,tStart, tEnd, bgui.threshold, bgui.sigma, bgui.verbose);
		}
		else{
			bgui.setLabel("Laplacian of Gaussian<br>"+title);
			if(bgui.verbose){bgui.log.print(title, "Running Laplacian of Gaussian");}
			map = processor.LoG(imp,bgui.mapC,tStart, tEnd, bgui.sigma, bgui.threshold, bgui.verbose);
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
					if(INDEX1D){
						frameBackgroundRoiArray[t].setPosition(t);
						boundaryBackgroundRoiArray[t].setPosition(t);
					}
					else{
						frameBackgroundRoiArray[t].setPosition(1, 1, t);
						boundaryBackgroundRoiArray[t].setPosition(1, 1, t);
					}
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
			Roi bodyRoi = new Roi(0,0,0,0);
			if(body.getRoi()!=null){
				bodyRoi = body.getRoi();
				bodyRoi.setStrokeColor(Color.RED);
				if(INDEX1D){
					bodyRoi.setPosition(tStart);
				}
				else{
					bodyRoi.setPosition(0,0,tStart);
				}
				prevol.add(bodyRoi);
			}
			IJ.run(proj, "Create Selection", "");
			if(proj.getRoi()!=null){
				Roi projRoi = proj.getRoi();
				projRoi.setStrokeColor(Color.PINK);
				if(INDEX1D){
					projRoi.setPosition(tStart);
				}
				else{
					projRoi.setPosition(0,0,tStart);
				}
				prevol.add(projRoi);
			}
			imp.setOverlay(prevol);
			return;
		}
		
		filo = new ArrayList<ArrayList<Filopart>>();
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
			ArrayList<Filopart> timeFilo = new ArrayList<Filopart>();
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
			if(INDEX1D){
				bodyRoi.setPosition(t);
			}
			else{
				bodyRoi.setPosition(bgui.mapC,1,t);
			}
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
			
			if(INDEX1D){
				bodyCentroid.setPosition(t);
			}
			else{
				bodyCentroid.setPosition(bgui.mapC,1,t);
			}
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
				
				
				
				if(INDEX1D){
					baseRoi.setPosition(t);
				}
				else{
					baseRoi.setPosition(0,1,t);
				}
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
				tipRoi.setPosition(0,1,t);
				if(INDEX1D){
					tipRoi.setPosition(t);
				}
				else{
					tipRoi.setPosition(0,1,t);
				}
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
				processRoi.setPosition(0,1,t);
				if(INDEX1D){
					processRoi.setPosition(t);
				}
				else{
					processRoi.setPosition(0,1,t);
				}
				processRoi.setStrokeColor(Color.CYAN);
				Filopart fp = new Filopart(processRoi,(Roi)baseRoi,tipRoi,pixelW,t,ind,area,baseMean,projMean,tipMean,tipThMean,bgui.sigma);
				timeFilo.add( fp );
			}
			filo.add(timeFilo);
		}
		IJ.run(imp, "Select None", "");	
		LinearAssigner LA1 = new LinearAssigner(imp,bgui.verbose);
		filo = LA1.run(filo);
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
					Filopart part = filo.get(tzb).get(f);
					if(part.index==i){
						if(lastT==tzb-1){
							Vector baseToTip = new Vector(part.baseCoord, part.tipCoord);
							Vector tipMove = new Vector(tipLast, part.tipCoord);
							Vector baseMove = new Vector(baseLast, part.baseCoord);
							part.dctm = tipMove.getRelativeMagnitude(baseToTip);
							part.dcbm = baseMove.getRelativeMagnitude(baseToTip);
						}
						tipLast = part.tipCoord;
						baseLast = part.baseCoord;
						lastT = tzb;
						continue time;
					}
				}
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	
	public void filtered(ArrayList<ArrayList<Filopart>> backPass){
		filo = backPass;
		LinearAssigner LA2 = new LinearAssigner(imp,bgui.verbose);
		filo = LA2.run(filo);
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
	
	public void trackEdited(ArrayList<ArrayList<Filopart>> backPass){
		filo = backPass;
		output(filo);
	}
	
	public void update(ArrayList<ArrayList<Filopart>> f){
		if(f.size()>=0){
			filo = f;
			doOverlay();
			imp.setOverlay(ol);
		}
	}
	
	public void doOverlay(){
	try{
		if(batch){return;}
		ol = new Overlay();
		bgui.setLabel("making overlay<br>"+title);
		if(bgui.verbose){bgui.log.print(title, "Making Overlay for "+title);}
		firstTrackIndex = Integer.MAX_VALUE;
		for(int t=0;t<filo.size();t++){	//make overlay
			if(bodyRoiArr[t]==null||filo==null||filo.size()==0){continue;}
			int index1D = t+1;//imp.getStackIndex(1, 1, t+1); //inconsistent behaviour
			if(INDEX1D){
				bodyRoiArr[t].setPosition(index1D);
			}
			else{
				bodyRoiArr[t].setPosition(0,1,t+1);
			}
			
			ol.add(bodyRoiArr[t]);
			for(int a=0;a<filo.get(t).size();a++){
				Filopart part = filo.get(t).get(a);
				if(part.index==-1){continue;}
				firstTrackIndex = (int)Math.min(firstTrackIndex, part.index);
				String str = String.valueOf(part.index);
				TextRoi label = new TextRoi( part.baseCoord.x/pixelW, part.baseCoord.y/pixelW, str, labelFont );
				
				if(INDEX1D){
					part.roi.setPosition(index1D);
					part.base.setPosition(index1D);
					part.tip.setPosition(index1D);
					label.setPosition(index1D);
				}
				else{
					part.roi.setPosition(0,1,t+1);
					part.base.setPosition(0,1,t+1);
					part.tip.setPosition(0,1,t+1);
					label.setPosition(0,1,t+1);
				}
				ol.add(part.roi);
				ol.add(part.base);
				ol.add(part.tip);

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
	
	public void output(final ArrayList<ArrayList<Filopart>> backPass){
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

					doOverlay();

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
									Filopart part = filo.get(t).get(f);
									if(part.index==i){
										if(start==Integer.MAX_VALUE){
											start = t;
											dtArr[t] = 0;
										}
										else{dtArr[t] = t-start;}
										baseMeanArr[t] = part.baseMean;
										projMeanArr[t] = part.projMean;
										tipMeanArr[t] = part.tipMean;
										tipThMeanArr[t] = part.tipThMean;

										imp.setPosition(bgui.measureC, 1, t);
										Roi tipBackgroundRoi = new ShapeRoi(RoiEnlargerHandler.enlarge( part.tip, 5 )).xor( new ShapeRoi(part.tip) ).not( new ShapeRoi(part.roi) ).not( new ShapeRoi(bodyRoiArr[t] ));
										if(tipBackgroundRoi!=null){
											imp.setRoi(tipBackgroundRoi);
											localTipBackgroundArr[t] = imp.getStatistics().mean;
											if(INDEX1D){
												tipBackgroundRoi.setPosition(t+1);
											}
											else{
												tipBackgroundRoi.setPosition(0,1,t+1);
											}
											localBackgroundRois.add(tipBackgroundRoi);
										}
										Roi baseBackgroundRoi = null;
										try{
											baseBackgroundRoi = new ShapeRoi(RoiEnlargerHandler.enlarge( part.base, 5 ))
																											.xor( new ShapeRoi(part.base) )
																											.not( new ShapeRoi(part.roi) )
																											.not( new ShapeRoi(bodyRoiArr[t] ));
										}catch(IllegalArgumentException iae){}	//ignore exception caused by zero area ShapeRoi, baseBackgroundRoi remains null
										if(baseBackgroundRoi!=null){
											imp.setRoi(baseBackgroundRoi);
											localBaseBackgroundArr[t] = imp.getStatistics().mean;
											if(INDEX1D){
												baseBackgroundRoi.setPosition(t+1);
											}
											else{
												baseBackgroundRoi.setPosition(0,1,t+1);
											}
											localBackgroundRois.add(baseBackgroundRoi);
										}

										lengthArr[t] = part.getLength();
										dctmArr[t] = part.dctm;
										dcbmArr[t] = part.dcbm;
										baseCoordArr[t] = part.baseCoord;
										tipCoordArr[t] = part.tipCoord;
										if(t>0){ dlArr[t] = lengthArr[t]-lengthArr[t-1]; }
										Roi backBase = new OvalRoi((part.baseCoord.x/pixelW)-4.5,(part.baseCoord.y/pixelW)-4.5,9,9);	//use the first real base coordinate for the first back projection
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
													
													if(INDEX1D){
														baseBackgroundRoi.setPosition(t+back+1);
													}
													else{
														baseBackgroundRoi.setPosition(0,1,t+back+1);
													}
													localBackgroundRois.add(baseBackgroundRoi);
												}

												if(INDEX1D){
													backBase.setPosition(t+back+1);
												}
												else{
													backBase.setPosition(0,1,t+back+1);
												}
												backBase.setStrokeColor(Color.ORANGE);
												backRoi.add(backBase);
												TextRoi backLabel = new TextRoi(backX,backY,""+i,labelFont);
												
												if(INDEX1D){
													backLabel.setPosition(t+back+1);
												}
												else{
													backLabel.setPosition(0,1,t+back+1);
												}
												backLabel.setStrokeColor(Color.ORANGE);
												backRoi.add(backLabel);
											}
											else{break;}
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
									//ctrow++;
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
						doOverlay();
					}
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
						bgui.log.print(title, "Bounder finished");
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
	
	public void run(String arg){
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
	
	//testing only
	public static void main(String[] arg){
		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Filopodyan test image...");
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
	    if(fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
	    	String path = fc.getSelectedFile().getAbsolutePath();
	    	ImagePlus image = new ImagePlus(path);
			image.show();
	    }
		
		final ij.ImageJ ij = new ij.ImageJ();
		
		ij.addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we){
				System.exit(1);
			}
		});
		
		new Filopodyan_().run("");
	}
	
}
