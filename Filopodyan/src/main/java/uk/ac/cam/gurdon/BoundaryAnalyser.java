package uk.ac.cam.gurdon;
import java.awt.Color;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Arrays;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.OvalRoi;
import ij.gui.Overlay;
import ij.gui.Plot;
import ij.gui.Roi;
import ij.gui.ShapeRoi;
import ij.plugin.Duplicator;
import ij.process.FloatProcessor;

/** Calculates and creates visualisations of local boundary shapes and velocities.
 * 
 * @author Richard Butler
 */
public class BoundaryAnalyser{
private FilopodyanGui bgui;
private ImagePlus imp;	
private int kymoWidth = 1000;

	/**
	 * @param bgui	The FilopodyanGui containing parameters
	 * @param imp	The ImagePlus from which Filoparts to be visualised were acquired
	 */
	public BoundaryAnalyser(FilopodyanGui bgui, ImagePlus imp){
		this.bgui = bgui;
		this.imp = imp;
	}
	
	/** Create the visualisations specified in the <code>FilopodyanGui</code>.
	 * 
	 * @param filo	The Filopart Collection to be visualised. This is a List of timepoints each having a List of FiloParts.
	 * @param bodyRoiArr	The growth cone body Rois to be visualised.
	 */
	public void run(ArrayList<ArrayList<FiloPod>> filo,ShapeRoi[] bodyRoiArr){
	try{
		int T = imp.getNFrames();
		String title = imp.getTitle();
		Duplicator dup = new Duplicator();
		ImagePlus map = dup.run(imp, 1, 1, 1, 1, 1, T);
		ImagePlus map2 = dup.run(imp, 1, 1, 1, 1, 1, T);
		
		Overlay velocityOl = new Overlay();
		Overlay signalOl = new Overlay();
		ArrayList<Float> velocity = new ArrayList<Float>();
		ArrayList<Float> signal = new ArrayList<Float>();
		ArrayList<Integer> time = new ArrayList<Integer>();
		ArrayList<Integer> position = new ArrayList<Integer>();
		int[] length = new int[T];
		double[] meanS = new double[T];
		double[] meanV = new double[T];
		double velocityMin = Double.POSITIVE_INFINITY;
		double velocityMax = Double.NEGATIVE_INFINITY;
		double signalMin = Double.POSITIVE_INFINITY;
		double signalMax = Double.NEGATIVE_INFINITY;
		int minLength = Integer.MAX_VALUE;
		int maxLength = -1;
		for(int t=1;t<=T;t++){
			ShapeRoi boundary = bodyRoiArr[t-1];
			for(FiloPod part : filo.get(t-1)){	//construct boundary Roi from included processes only
				boundary = boundary.or(new ShapeRoi(part.getRoi()));
			}
			imp.setRoi(boundary);	
			IJ.run(imp, "Interpolate", "interval=1");
			Polygon poly = imp.getRoi().getPolygon();
			length[t-1] = poly.npoints;
			IJ.run(imp, "Select None", "");
			if(t>1&&t<T){
				bgui.setLabel("boundary T"+t+"<br>"+title);
				for(int i=0;i<poly.npoints;i++){
					time.add(t-1);
					position.add(i);
					
					//adaptive signal
					imp.setPosition(bgui.measureC,1,t);
					Roi inner = new ShapeRoi(boundary).and(new ShapeRoi(new OvalRoi(poly.xpoints[i]-4.5d,poly.ypoints[i]-4.5d,9d,9d)));
					imp.setRoi(inner);
					Double mean = imp.getStatistics().mean;
					float sf = new Float(mean);
					signal.add(sf);
					meanS[t] += sf;
					signalMax = Math.max(signalMax,sf);
					signalMin = Math.min(signalMin,sf);
					Roi innerMarker = new Roi(poly.xpoints[i]-0.5d,poly.ypoints[i]-0.5d,1d,1d);
					innerMarker.setPosition(bgui.measureC,1,t);
					signalOl.add(innerMarker);
					
					//adaptive velocity
					imp.setPosition(bgui.mapC,1,t-1);
					imp.setRoi(new Roi(poly.xpoints[i]-1,poly.ypoints[i]-1,3,3));
					double pixb1 = imp.getStatistics().mean;
					imp.setPosition(bgui.mapC,1,t);
					double pix0 = imp.getStatistics().mean;
					imp.setPosition(bgui.mapC,1,t+1);
					double pixf1 = imp.getStatistics().mean;
					IJ.run(imp, "Select None", "");
					double gradient = ((pixb1-pix0)/2) + ((pix0-pixf1)/2);
					Roi marker = new Roi(poly.xpoints[i]-0.5d,poly.ypoints[i]-0.5d,1d,1d);
					marker.setPosition(bgui.measureC,1,t);
					float vf = new Float(gradient);
					velocity.add(vf);
					meanV[t] += vf;
					velocityMax = Math.max(velocityMax,vf);
					velocityMin = Math.min(velocityMin,vf);
					velocityOl.add(marker);
					
				}
				meanS[t] = meanS[t]/poly.npoints;
				meanV[t] = meanS[t]/poly.npoints;
			}
			
			maxLength = Math.max(maxLength,poly.npoints);
			minLength = Math.max(minLength,poly.npoints);
		}
		
		for(int i=0;i<velocityOl.size();i++){
			float normV = new Float( (velocity.get(i)-velocityMin)/(velocityMax-velocityMin) );
			if(normV<0f){normV=0f;}
			else if(normV>1f){normV=1f;}
			velocity.set(i,normV); 
			velocityOl.get(i).setFillColor(new Color( normV, 1f-normV, 0f ));
			
			float normS = new Float( (signal.get(i)-signalMin)/(signalMax-signalMin) );
			if(normS<0f){normS=0f;}
			else if(normS>1f){normS=1f;}
			signal.set(i,normS); 
			signalOl.get(i).setFillColor(new Color( 0f, normS, 0f ));
		}
		
		IJ.run(imp, "Select None", "");
		IJ.setBackgroundColor(0, 0, 0);
		IJ.run(map, "Select All", "");
		IJ.run(map, "Clear", "stack");
		IJ.run(map, "Select None", "");
		map.setOverlay(velocityOl);
		map.setTitle("Boundary Velocity : "+title);
		map.changes = false;
		map.show();
		
		IJ.run(map2, "Select All", "");
		IJ.run(map2, "Clear", "stack");
		IJ.run(map2, "Select None", "");
		map2.setOverlay(signalOl);
		map2.setTitle("Boundary C"+bgui.measureC+" signal : "+title);
		map2.changes = false;
		map2.show();
	
		FloatProcessor fpS = new FloatProcessor(kymoWidth,T-2);
		FloatProcessor fpV = new FloatProcessor(kymoWidth,T-2);
	if(bgui.kymographs){
		if(bgui.verbose){bgui.log.print(title, "Making kymographs");}
		for(int i=0;i<signal.size()-1;i++){
			int thisTime = time.get(i);
			if(length[thisTime]==0) continue;
			double x = ((double)position.get(i)/length[thisTime]);
			int normPos = (int)Math.round(x*kymoWidth);
			fpS.setf(normPos,thisTime-1,signal.get(i));
			fpV.setf(normPos,thisTime-1,velocity.get(i)-0.5f);	//set 0 as no movement, so -ve is retraction and +ve is extension	
		}
		ImagePlus kymoS = new ImagePlus(title+"C"+bgui.measureC+" Boundary Signal Kymograph",fpS);
		kymoS.show();
		ImagePlus kymoV = new ImagePlus(title+"C"+bgui.mapC+" Boundary Velocity Kymograph",fpV);
		kymoV.show();
	}
	if(bgui.kymographs&&bgui.ccf){
		int dpMax = 0;	//maximum deltas for CCF
		int dtMax = 10;
		double[] ccf = new double[dtMax+1];
		double ccfMin = Double.POSITIVE_INFINITY;
		double ccfMax = Double.NEGATIVE_INFINITY;
		double[] deltaT = new double[dtMax+1];
		for(int dp=-dpMax;dp<=dpMax;dp++){
		bgui.setLabel("CCF "+(dp+dpMax)+"/"+(dpMax+dpMax)+"<br>"+title);
		if(bgui.verbose){bgui.log.print(title, "CCF "+(dp+dpMax)+"/"+(dpMax+dpMax));}
		for(int dt=-dtMax;dt<=0;dt++){
			double num = 0d;
			double denom1 = 0d;
			double denom2 = 0d;
			for(int p=dpMax;p<kymoWidth-dpMax-2;p++){
				for(int t=dtMax;t<T-dtMax-2;t++){
					float sig = fpS.get(p,t);
					float vel = fpV.get(p+dp,t+dt);
					if(sig>0.25){
						num += ((sig-meanS[t])*(vel-meanV[t+dt]));
						denom1 += Math.abs(sig-meanS[t]);
						denom2 += Math.abs(vel-meanV[t+dt]);
					}
				}
			}
			ccf[dtMax+dt] = num/(denom1*denom2);
			ccfMin = Math.min(ccfMin,ccf[dt+dtMax]);
			ccfMax = Math.max(ccfMax,ccf[dt+dtMax]);
			deltaT[dtMax+dt] = dt;
		}
		}
		Plot CCFplot = new Plot(title+" CCF boundary signal*velocity", "\u0394T" ,"r");
		CCFplot.setFrameSize(800, 800);
		CCFplot.setLimits(-dtMax,0,ccfMin*0.9,ccfMax*1.1);
		CCFplot.addPoints(deltaT,ccf,Plot.LINE);
		CCFplot.show();
	}	
	}catch(Exception e){IJ.log(e.toString()+"\n~~##~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}

}
