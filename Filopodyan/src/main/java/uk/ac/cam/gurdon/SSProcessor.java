package uk.ac.cam.gurdon;

import java.util.Arrays;

import ij.ImagePlus;
import ij.ImageStack;
import ij.gui.Roi;
import ij.gui.ShapeRoi;
import ij.plugin.Duplicator;
import ij.plugin.HyperStackConverter;
import ij.plugin.filter.RankFilters;
import ij.plugin.filter.ThresholdToSelection;
import ij.process.AutoThresholder;
import ij.process.Blitter;
import ij.process.ByteProcessor;
import ij.process.FloatProcessor;
import ij.process.ImageProcessor;
import ij.process.ImageStatistics;

public class SSProcessor implements FilopodyanProcessor {

	
	/**Scale Space processing and bi-directional Kalman filtering
	 * 
	 * @param imp	the image to process, not changed by the processing
	 * @param chan	the channel to process
	 * @param t0	the start index of the time range to process (inclusive)
	 * @param t1	the end index of the time range to process (inclusive)
	 * @param sigma	the Gaussian sigma value
	 * @param threshold	the ImageJ thresholding method to use to create the binary mask from the processed image
	 * @param verbose	true to log additional information including the LoG kernel
	 * @return	the mask
	 * */
	@Override
	public ImagePlus process(ImagePlus imp, int chan, int t0, int t1, double basesigma, String thresholdMethod, boolean verbose) {

		imp.killRoi();
		ImagePlus map = new Duplicator().run(imp,chan,chan, 1, 1, t0, t1);
		int T = t1-t0+1;
		AutoThresholder.Method method = AutoThresholder.Method.valueOf(thresholdMethod);
		FloatProcessor[] detections = new FloatProcessor[T];
		//double[] sigmas = new double[]{1,2,3,4};
		double[] sigmas = new double[]{basesigma*0.5,basesigma,basesigma*1.5,basesigma*2,basesigma*3};
		double[] ks = new double[]{0,2,4,6};
		for(int t=0;t<T;t++){
			detections[t] = new FloatProcessor(imp.getWidth(), imp.getHeight());
			for(double sigma:sigmas){
				for(double k:ks){
					ImageProcessor smask = segment(map, t, sigma,k, method);
					detections[t].copyBits(smask, 0,0, Blitter.ADD);
				}
			}
			detections[t].multiply(1./(255.0*sigmas.length*ks.length));
		}
		
		int width = imp.getWidth();
		int height = imp.getHeight();
		int n = width*height;
		
		// bi-directional Kalman
		boolean doKalman = false;
		if(doKalman){
			double noise = 0.1;
			double gain = 0.6;
			ImageProcessor[] ips = new ImageProcessor[detections.length];
			for(int t=0;t<T;t++){
				ips[t] = detections[t].duplicate();
			}
			float[] predictedvar = new float[n];
			float[] corrected = new float[n];
			float[] correctedvar = new float[n];
			float[] predicted = (float[]) ips[0].getPixels();
			//FloatProcessor[] proc = new FloatProcessor[ips.length];
			for(int t=0;t<T;t++){ // forward
				float[] observed = (float[]) ips[t].getPixels();
				for(int k=0;k<n;k++){
					double kalman = predictedvar[k]/(predictedvar[k]+noise);
					corrected[k] = (float) (gain*predicted[k]+(1.0-gain)*observed[k]+kalman*(observed[k] - predicted[k]));
					correctedvar[k] = (float) (predictedvar[k]*(1.0 - kalman));
				}
				predictedvar = correctedvar;
				predicted = corrected;
				detections[t] = new FloatProcessor(width, height, corrected);
			}
			for(int t=0;t<T;t++){ // reverse
				float[] observed = (float[]) detections[map.getStackSize()-t-1].getPixels();
				for(int k=0;k<n;k++){
					double kalman = predictedvar[k]/(predictedvar[k]+noise);
					corrected[k] = (float) (gain*predicted[k]+(1.0-gain)*observed[k]+kalman*(observed[k] - predicted[k]));
					correctedvar[k] = (float) (predictedvar[k]*(1.0 - kalman));
				}
				predictedvar = correctedvar;
				predicted = corrected;
				detections[T-t-1] = new FloatProcessor(width, height, corrected);
			}
		}
		
		double minP = 0.1;
		ImageStack outStack = new ImageStack(width, height);
		for(int t=0;t<T;t++){
			ImageProcessor ip = detections[t];
			ByteProcessor bp = new ByteProcessor(width, height);
			for(int i=0;i<width*height;i++){
				bp.set(i, ip.get(i)>=minP?255:0);
			}
			outStack.addSlice(bp);
		}
		ImagePlus procImp = new ImagePlus("SSProc", outStack);
//procImp.show();
		procImp.setDimensions(1,1,T);
		return procImp;
	}

	private ByteProcessor segment(ImagePlus imp, int t, double sigma, double k, AutoThresholder.Method method){
		ImageProcessor proc = imp.getStack().getProcessor(t+1).duplicate();
		proc.log();
		ImageProcessor sub = proc.duplicate();
		proc.blurGaussian(sigma);
		if(k>0){
			sub.blurGaussian(k*sigma);
			proc.copyBits(sub, 0,0, Blitter.SUBTRACT);
		}
		ImageStatistics stats = proc.getStatistics();
		double thresh = new AutoThresholder().getThreshold( method, stats.histogram );
		thresh = (thresh/255f) * (stats.max-stats.min) + stats.min;
		proc.threshold((int)thresh);
		ByteProcessor mask = proc.convertToByteProcessor(false);
		return mask;
	}
	
}
