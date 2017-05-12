import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import ij.IJ;
import ij.Prefs;
import ij.measure.Calibration;

public class FiloFilter implements ActionListener{
private JFrame gui;
private JTextField startField, framesField, lengthField, dlField, wavinessField, dctmField, dcbmField;
private Filopodyan_ parent;
private int frames = (int)Math.round(Prefs.get("Filopodyan.frames",1));
private double length = Prefs.get("Filopodyan.length",0d);
private double dl = Prefs.get("Filopodyan.dl",0d);
private double dctm = Prefs.get("Filopodyan.dctm",0d);
private double dcbm = Prefs.get("Filopodyan.dcbm",0d);
private double waviness = Prefs.get("Filopodyan.waviness",1d);
private int start = (int)Math.round(Prefs.get("Filopodyan.start",1));
private int maxIndex;
private String unit;
private double pixW;
private ArrayList<ArrayList<Filopart>> filo, original;

	public FiloFilter(ArrayList<ArrayList<Filopart>> filo, int maxIndex, Filopodyan_ parent){
	try{
		this.filo = filo;
		this.maxIndex = maxIndex;
		this.parent = parent;
		Calibration cal = parent.imp.getCalibration();
		this.pixW = cal.pixelWidth;
		this.unit = cal.getUnit();
		this.original = new ArrayList<ArrayList<Filopart>>();
			for(int t=0;t<filo.size();t++){
				ArrayList<Filopart> copy = new ArrayList<Filopart>();
				for(int p=0;p<filo.get(t).size();p++){
					copy.add(new Filopart(filo.get(t).get(p)));
				}
				original.add(copy);
			}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void run(){
	try{
		if(parent.bgui.verbose){parent.bgui.log.print(parent.imp.getTitle(), "Filopodyan Filter...");}
		gui = new JFrame("Filopodyan Filter");
		gui.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("logo_icon.gif")));
		gui.setLayout(new BorderLayout());
		JPanel fieldPan = new JPanel(new GridLayout(0,2,2,2));
		fieldPan.add(new JLabel("Min start frame",JLabel.RIGHT));
		startField = new JTextField(""+start,3);
		fieldPan.add(startField);
		fieldPan.add(new JLabel("Min frames",JLabel.RIGHT));
		framesField = new JTextField(""+frames,3);
		fieldPan.add(framesField);
		fieldPan.add(new JLabel("Min max length ("+unit+")",JLabel.RIGHT));
		lengthField = new JTextField(""+length,3);
		fieldPan.add(lengthField);
		fieldPan.add(new JLabel("Min length change ("+unit+")",JLabel.RIGHT));
		dlField = new JTextField(""+dl,3);
		fieldPan.add(dlField);
		fieldPan.add(new JLabel("Min max DCTM ("+unit+")",JLabel.RIGHT));
		dctmField = new JTextField(""+dctm,3);
		fieldPan.add(dctmField);
		fieldPan.add(new JLabel("Min max DCBM ("+unit+")",JLabel.RIGHT));
		dcbmField = new JTextField(""+dcbm,3);
		fieldPan.add(dcbmField);
		fieldPan.add(new JLabel("Max mean waviness",JLabel.RIGHT));
		wavinessField = new JTextField(""+waviness,3);
		fieldPan.add(wavinessField);
		gui.add(fieldPan,BorderLayout.CENTER);
		JPanel buttonPan = new JPanel();
		JButton applyButton = new JButton("Apply");
		applyButton.addActionListener(this);
		buttonPan.add(applyButton);
		JButton revertButton = new JButton("Remove");
		revertButton.addActionListener(this);
		buttonPan.add(revertButton);
		JButton doneButton = new JButton("Done");
		doneButton.addActionListener(this);
		buttonPan.add(doneButton);
		gui.add(buttonPan,BorderLayout.SOUTH);
		gui.pack();
		Rectangle bounds = parent.imp.getWindow().getBounds();
		gui.setLocation(bounds.x+bounds.width,bounds.y+50);
		gui.setVisible(true);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void batchFilter(BatchFilopodyan bgui){
	try{
		this.start = bgui.start;
		this.frames = bgui.frames;
		this.length = bgui.length;
		this.dl = bgui.dl;
		this.dctm = bgui.dctm;
		this.dcbm = bgui.dcbm;
		this.waviness = bgui.waviness;
		ArrayList<ArrayList<Filopart>> result = filter();
		parent.filtered(result);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	private ArrayList<ArrayList<Filopart>> filter(){
		ArrayList<ArrayList<Filopart>> ff = new ArrayList<ArrayList<Filopart>>();
	try{
		int T = filo.size();
		for(int i=0;i<maxIndex;i++){
			ArrayList<ArrayList<Filopart>> toAdd = new ArrayList<ArrayList<Filopart>>();
			int exists = 0;
			double longest = 0d;
			double changest = 0d;
			double dctmest = 0d;
			double dcbmest = 0d;
			double meanWaviness = 0d;
			int first = Integer.MIN_VALUE;
			double firstLength = Double.NEGATIVE_INFINITY;
			for(int t=0;t<T;t++){
				toAdd.add(new ArrayList<Filopart>());
				for(int f=0;f<filo.get(t).size();f++){
					Filopart part = filo.get(t).get(f);
					if(part.index==i){
						if(first==Integer.MIN_VALUE){
							first = t+1;
							firstLength = part.getLength();
						}
						toAdd.get(t).add(part);
						
						double len = part.getLength();
	
						longest = Math.max(longest,len);
						changest = Math.max(changest,Math.abs(len-firstLength));
						dctmest = Math.max(dctmest,part.dctm);
						dcbmest = Math.max(dcbmest,part.dcbm);
						
						double straightL = part.baseCoord.distance(part.tipCoord);
						double straightness = straightL/len;
						if(straightness>1d){straightness = 1d;}
						double wav = (1d-straightness);

						meanWaviness += wav;
						
						exists++;
					}
				}
			}
			meanWaviness /= exists;
			if(parent.bgui.verbose){
				parent.bgui.log.print(parent.imp.getTitle(), "track "+i+" : first<"+first+"~"+start+"> longest<"+longest+"~"+length+"> frames<"+exists+"~"+frames+"> dl<"+changest+"~"+(dl/pixW)+"> dctm<"+dctmest+"~"+dctm+"> waviness<"+meanWaviness+"~"+waviness+">");
			}
			if( (first>=start)&&(longest>=length)&&(exists>=frames)&&(changest>=dl)&&(dctmest>=dctm)&&(dcbmest>=dcbm)&&(meanWaviness<=waviness) ){
				for(int t=0;t<T;t++){
					if(ff.size()<=t){ff.add(new ArrayList<Filopart>());}
					for(int f=0;f<toAdd.get(t).size();f++){
						ff.get(t).add(toAdd.get(t).get(f));
						if(parent.bgui.verbose){parent.bgui.log.print(parent.imp.getTitle(), "Including object "+f+" at T"+t+" in track "+i);}
					}
				}
			}
			else if(parent.bgui.verbose){parent.bgui.log.print(parent.imp.getTitle(), "Excluding track "+i+" frames="+exists+", longest="+longest+" dl="+changest+" mean waviness="+meanWaviness);}
		}
		parent.update(ff);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return ff;
	}
	
	public void actionPerformed(ActionEvent ae){
	try{
		String event = ae.getActionCommand();
		if(event=="Apply"){
			try{
				start = Integer.parseInt(startField.getText());
			}catch(NumberFormatException nfe){IJ.error(startField.getText()+" is not an integer.");return;}
			try{
				frames = Integer.parseInt(framesField.getText());
			}catch(NumberFormatException nfe){IJ.error(framesField.getText()+" is not an integer.");return;}
			try{
				length = Double.valueOf(lengthField.getText());
			}catch(NumberFormatException nfe){IJ.error(lengthField.getText()+" is not a number.");return;}
			try{
				dl = Double.valueOf(dlField.getText());
			}catch(NumberFormatException nfe){IJ.error(dlField.getText()+" is not a number.");return;}
			try{
				dctm = Double.valueOf(dctmField.getText());
			}catch(NumberFormatException nfe){IJ.error(dctmField.getText()+" is not a number.");return;}
			try{
				dcbm = Double.valueOf(dcbmField.getText());
			}catch(NumberFormatException nfe){IJ.error(dcbmField.getText()+" is not a number.");return;}
			try{
				waviness = Double.valueOf(wavinessField.getText());
			}catch(NumberFormatException nfe){IJ.error(wavinessField.getText()+" is not a number.");return;}
			filo = original;
			filo = filter();
			parent.update(filo);
			Prefs.set("Filopodyan.start",start);
			Prefs.set("Filopodyan.frames",frames);
			Prefs.set("Filopodyan.length",length);
			Prefs.set("Filopodyan.dl",dl);
			Prefs.set("Filopodyan.dctm",dctm);
			Prefs.set("Filopodyan.dcbm",dcbm);
			Prefs.set("Filopodyan.waviness",waviness);
		}
		else if(event=="Remove"){
			if(parent.bgui.verbose){parent.bgui.log.print(parent.imp.getTitle(), "Removing filter");}
			filo = original;	//filter doesn't modify the filo lists
			parent.update(filo);
		}
		else if(event=="Done"){
			String str = "Filter settings-\n"+
						 "Min start frame: "+start+"\n"+
						 "Min frames: "+frames+"\n"+
						 "Min max length: "+length+"\n"+
						 "Min length change: "+dl+"\n"+
						 "Min max DCTM: "+dctm+"\n"+
						 "Min max DCBM: "+dcbm+"\n"+
						 "Max mean waviness: "+waviness+"\n";
			parent.bgui.log.print(parent.imp.getTitle(), str);
			gui.dispose();
			parent.filtered(filo);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}

}
