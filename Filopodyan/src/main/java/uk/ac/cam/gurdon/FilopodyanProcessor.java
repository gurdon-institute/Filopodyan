package uk.ac.cam.gurdon;

import ij.ImagePlus;

/** Implemented by classes for processing an image to create a binary mask
 * 
 * @author Richard Butler
 */
public interface FilopodyanProcessor {
	
	/**The method implementing processing
	 * 
	 * @param imp	the image to process, not changed by the processing
	 * @param chan	the channel to process
	 * @param t0	the start index of the time range to process (inclusive)
	 * @param t1	the end index of the time range to process (inclusive)
	 * @param sigma	the Gaussian sigma value
	 * @param threshold	the ImageJ thresholding method to use to create the binary mask from the processed image
	 * @param verbose	true to log additional information including the LoG kernel
	 * @return	a binary mask created by this method
	 * */
	public ImagePlus process(ImagePlus imp, int chan, int t0, int t1, double sigma, String threshold, boolean verbose);
	
}
