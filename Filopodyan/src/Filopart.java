import java.awt.Rectangle;

import javax.vecmath.Point2d;

import ij.gui.Roi;

public class Filopart{
	Roi roi,base,tip;
	int T;
	int index;
	double pixW,area,baseMean,projMean,tipMean,tipThMean,joinCost,dctm,dcbm,sigma,length;
	Point2d baseCoord, tipCoord;
	
	public Filopart(Roi r, Roi base, Roi tip, double pixW, int t, int i, double a, double meanB, double meanP, double meanT, double meanThT, double sigma){
		this.roi = r;
		this.pixW = pixW;
		this.T = t;
		this.index = i;
		this.area = a;
		this.base = base;
		this.tip = tip;
		this.baseMean = meanB;
		this.projMean = meanP;
		this.tipMean = meanT;
		this.tipThMean = meanThT;
		Rectangle rectb = base.getBounds();
		this.baseCoord = new Point2d( (rectb.x+(rectb.width/2))*pixW, (rectb.y+(rectb.height/2))*pixW);
		Rectangle rectt = tip.getBounds();
		this.tipCoord = new Point2d( (rectt.x+(rectt.width/2))*pixW, (rectt.y+(rectt.height/2))*pixW);
		this.joinCost = Double.MAX_VALUE;
		this.dctm = 0d;
		this.dcbm = 0d;
		this.sigma = sigma;
		this.length = Double.NaN;
	}
	
	public Filopart(Filopart copy){
		this.roi = (Roi)copy.roi.clone();
		this.pixW = copy.pixW;
		this.T = copy.T;
		this.index = copy.index;
		this.area = copy.area;
		this.base = copy.base;
		this.tip = copy.tip;
		this.baseMean = copy.baseMean;
		this.projMean = copy.projMean;
		this.tipMean = copy.tipMean;
		this.tipThMean = copy.tipThMean;
		this.baseCoord = copy.baseCoord;
		this.tipCoord = copy.tipCoord;
		this.joinCost = copy.joinCost;
		this.dctm = copy.dctm;
		this.dcbm = copy.dcbm;
		this.sigma = copy.sigma;
		this.length = copy.length;
	}
	
	public double getLength(){	//estimate length as half perimeter corrected for width
		if(Double.isNaN(length)){
			double w = 2 * Math.sqrt(2*Math.log(2)) * sigma * pixW;
			double EC = ((Math.PI - 2)/2) * w;
			double D = baseCoord.distance(tipCoord);
			double SF = Math.min( ((D/w)*(D/w)), 1d );
			length = ((roi.getLength()/2)*pixW) - (EC * SF);
		}
		return length;
	}
	
}
