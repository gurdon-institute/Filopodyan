package uk.ac.cam.gurdon;

import java.util.Arrays;

import ij.IJ;
import ij.ImagePlus;
import ij.ImageStack;
import ij.Prefs;
import ij.plugin.Duplicator;
import ij.plugin.ImageCalculator;
import ij.plugin.filter.Convolver;
import ij.process.ImageProcessor;
import ij.process.ShortProcessor;

/** Adaptive Local Thresholding using directional LoG processing
 * 
 * @author Richard Butler
 */
public class ALTProcessor implements FilopodyanProcessor{
	
	/** Adaptive Local Thresholding using directional LoG processing. Pre-processes with the ImageJ rolling ball.
	 * 
	 * @param imp	the image to process, not changed by the processing
	 * @param chan	the channel to process
	 * @param t0	the start index of the time range to process (inclusive)
	 * @param t1	the end index of the time range to process (inclusive)
	 * @param sigma	the Gaussian sigma value for directional LoG kernels, rolling ball radius is set to 10*sigma
	 * @param threshold	the ImageJ thresholding method to use to create the binary mask from the processed image
	 * @param verbose	true to log additional information including the directional LoG kernels
	 * @return	a mask created by LoG processing and thresholding using the sigma and autothreshold method values given as arguments
	 */
	public ImagePlus process(ImagePlus imp, int chan, int t0, int t1, double sigma, String threshold, boolean verbose){
			ImagePlus map = new Duplicator().run(imp,chan,chan, 1, 1, t0, t1);
			IJ.run(map, "Subtract Background...", "rolling="+(sigma*10)+" stack");
			
			int W = imp.getWidth();
			int H = imp.getHeight();
			Convolver conv = new Convolver();
			conv.setNormalize(true);
			
			int radius = (int)Math.ceil(2*sigma);
			int diam = 2*radius+1;
			float[][] k = new float[8][(int)Math.pow(diam,2)];
			
			for(int i=0;i<8;i++){	//make directional kernels for the 8 principal directions
				int pos = 0;
				for(int y=-radius;y<=radius;y++){
					for(int x=-radius;x<=radius;x++){
							 if(i==0&&(x<0)){k[i][pos]=0f;}
						else if(i==1&&(x<0||y<0)){k[i][pos]=0f;}
						else if(i==2&&(y<0)){k[i][pos]=0f;}
						else if(i==3&&(x>0||y>0)){k[i][pos]=0f;}
						else if(i==4&&(x>0)){k[i][pos]=0f;}
						else if(i==5&&(x>0||y<0)){k[i][pos]=0f;}
						else if(i==6&&(y>0)){k[i][pos]=0f;}
						else if(i==7&&(x<0||y>0)){k[i][pos]=0f;}
						else{	//LoG in one direction
							k[i][pos] = (float)( (-1d/(Math.PI*Math.pow(sigma,4))) * (1d - ((x*x + y*y)/ (2*sigma*sigma))) * Math.exp(-((x*x + y*y) / (2*sigma*sigma))) );	
						}
						pos++;
					}
				}
				if(verbose){FilopodyanLog.get().print(imp.getTitle(), "Kernel "+i+" :\n"+Arrays.toString(k[i]));}
			}
			
			ImageStack stack = new ImageStack(W,H);
			for(int t=t0;t<=t1;t++){
				if(verbose){FilopodyanLog.get().print(imp.getTitle(), "Adaptive Thresholding T"+t);}
				imp.setPosition(chan,1,t);
				ImagePlus dup = new Duplicator().run(map, chan,chan, 1, 1, t, t);
				int[][] values = dup.getProcessor().getIntArray();
				dup.close();
				ImageProcessor[] ip = new ImageProcessor[8];
				ImagePlus out = new ImagePlus("ALT",new ShortProcessor(W,H));
				ImageCalculator ic = new ImageCalculator();
				for(int i=0;i<8;i++){
					ip[i] = new ShortProcessor(W,H);
					ip[i].setIntArray(values);
					ip[i].convolve(k[i],diam,diam);
					ImagePlus result = new ImagePlus(""+i,ip[i]);
					IJ.run(result, "Median...", "radius=2");
					Prefs.blackBackground = true;
					IJ.setAutoThreshold(result, threshold+" dark stack");
					IJ.run(result, "Convert to Mask", "");
					ImagePlus temp = ic.run("Or create",out,result);
					out.setProcessor(temp.getProcessor());
					//result.show();
				}
				IJ.run(out, "Convert to Mask", "stack");
				IJ.run(out, "Close-", "stack");
				IJ.run(out, "Open", "stack");
				IJ.run(out, "Remove Outliers...", "radius=5 threshold=0 which=Bright stack");
				//out.show();
				stack.addSlice(out.getProcessor());
			}
			
			ImagePlus image = new ImagePlus(imp.getTitle(),stack);
			IJ.run(image, "Properties...", "channels=1 slices=1 frames="+(t1-t0+1));
			//image.show();
			return image;
	}
}
