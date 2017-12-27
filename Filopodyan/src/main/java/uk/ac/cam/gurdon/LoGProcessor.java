package uk.ac.cam.gurdon;
import ij.IJ;
import ij.ImagePlus;
import ij.plugin.Duplicator;
import ij.plugin.filter.Convolver;

/** LoG processing to create a binary mask
 * 
 * @author Richard Butler
 */
public class LoGProcessor implements FilopodyanProcessor{

/**LoG processing
 * 
 * @param imp	the image to process, not changed by the processing
 * @param chan	the channel to process
 * @param t0	the start index of the time range to process (inclusive)
 * @param t1	the end index of the time range to process (inclusive)
 * @param sigma	the Gaussian sigma value
 * @param threshold	the ImageJ thresholding method to use to create the binary mask from the processed image
 * @param verbose	true to log additional information including the LoG kernel
 * @return	a mask created by LoG processing and thresholding using the sigma and autothreshold method values given as arguments
 * */
public ImagePlus process(ImagePlus imp, int chan, int t0, int t1, double sigma, String threshold, boolean verbose){
	
	ImagePlus map = new Duplicator().run(imp,chan,chan, 1, 1, t0, t1);
	int radius = (int)Math.ceil(2*sigma);
	int diam = 2*radius+1;
	float[] kernel = new float[(int)Math.pow(diam,2)];
	int pos = 0;
	StringBuilder kernelStr = new StringBuilder("LoG Kernel:\n");
	for(int y=-radius;y<=radius;y++){
		for(int x=-radius;x<=radius;x++){
			kernel[pos] = (float)( (-1d/(Math.PI*Math.pow(sigma,4))) * (1d - ((x*x + y*y)/ (2*sigma*sigma))) * Math.exp(-((x*x + y*y) / (2*sigma*sigma))) );			
			if(verbose){kernelStr.append((x>-radius?" ":"")+String.format("%f6" ,kernel[pos]));}
			pos++;
		}
		if(verbose){kernelStr.append((y<radius?"\n":""));}
	}
	if(verbose){FilopodyanLog.get().print(imp.getTitle(), kernelStr.toString());}
	Convolver conv = new Convolver();
	IJ.run(map, "32-bit", "");
	for(int m=1;m<=map.getStackSize();m++){
		if(verbose){FilopodyanLog.get().print(imp.getTitle(), "LoG "+m);}
		map.setPosition(m);
		conv.convolveFloat(map.getProcessor(),kernel,diam,diam);	 //calculate values to float precision and set ip to FloatProcessor in place
	}
	IJ.setAutoThreshold(map, threshold+" dark stack");
	IJ.run(map, "Convert to Mask", "method="+threshold+" background=Dark black");
	IJ.run(map, "Fill Holes", "stack");
	IJ.run(map, "Open", "stack");
	return map;
}

}
