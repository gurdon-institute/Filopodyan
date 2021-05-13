package uk.ac.cam.gurdon;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.Utilities;

import ij.IJ;
import ij.ImagePlus;
import ij.Prefs;

/** The main GUI and parameter store for Filopodyan.
 * 
 * @author Richard Butler
 */
public class FilopodyanGui extends JDialog implements ActionListener, ChangeListener{
private static final long serialVersionUID = 9710347002937l;
public JCheckBox boundaryTick, tipPlotTick, filoTableTick, coordTableTick, bodyTableTick, kymographsTick,
				 ccfTick, basePlotTick, timeTick, verboseTick, adaptiveTick, fitTick, joinTick, showBackgroundTick,
				 processProfileTick;
public JToggleButton advTick;
public JComboBox<String> mapCombo, measureCombo, thresholdCombo;
public JTextField itField, sigmaField, weightField, dWField, oWField, backField;
public JSlider timeSlider;
public SwingWorker<Object, Void> workGui;
public Filopodyan_ parent;
public FilopodyanLog log;
private ImagePlus target;
private ArrayList<Component> advList;
public static final String helpText = 
"<html>"+
"<body style=\"font-family:arial,sans-serif;font-size:14pt;font-weight:normal;background-color:white;padding:10px;\">"+
"<h3 align=\"center\" style=\"font-size:18pt;\">Filopodyan Help</h3>"+
"<p><b>Filopo</b>dia <b>Dy</b>namics <b>An</b>alysis<br>"+

"<p>"+
"<a href=\"http://jcb.rupress.org/content/216/10/3405\">Filopodyan: An open-source pipeline for the analysis of filopodia</a><br>"+
"<span style=\"font-size:12pt;\">Vasja Urbančič, Richard Butler, Benjamin Richier, Manuel Peter, Julia Mason, Frederick J. Livesey, Christine E. Holt, Jennifer L. Gallop</span><br>"+
"<span style=\"font-size:12pt;font-style:italic;\">J Cell Biol Oct 2017, 216 (10) 3405-3422; DOI: 10.1083/jcb.201705113</span><br>"+
"<br></p>"+


"Filopodyan maps growth cone boundaries, identifies processes and tracks them over time. It uses LoG processing to enhance local intensity gradients, "+
"a choice of ImageJ thresholding algorithms to create binary masks, erosion followed by dilation to segment processes from the cell body, and "+
"tracks processes over time using a rapid one-step linear assignment algorithm. Tracked processes can be filtered by various parameters and then "+
"manually deleted or joined if necessary."+
"<ul><b>Basic</b>"+
"<li>Map C: The channel to use for process mapping.</li>"+
"<li>Measure C: The channel to use for intensity measurement.</li>"+
"<li>Threshold: Thresholding algorithm used for mapping.</li>"+
"<li>ED Iterations: The number of Erode/Dilate operations to apply to segment processes.</li>"+
"<li>LoG sigma: Sigma value for Laplacian of Gaussian pre-processing.</li>"+
"<li>Base back frames: The number of previous frames in which to predict process base positions before formation events.</li>"+
"</ul><ul><b>Advanced</b>"+
"<li>Adaptive thresholding: Apply adaptive local thresholding to enhance detection when signal levels above background are highly variable."+
"Uses 8 directional LoG operations to create boundary images which are thresholded individually, filtered then combined.</li>"+
"<li>Fit tip to measure C: Sets process tip locations based on the mapping channel and then adjusts them to fit the positions of local maxima in the measurement channel.</li>"+
"<li>Join fragments: Joins isolated boundary fragments to the closest point on the cell boundary.</li>"+
"</ul><ul><b>Filopodyan Filter</b>"+
"<li>Min start frame: the earliest frame to include.</li>"+
"<li>Min frames: the number of frames processes exist for.</li>"+
"<li>Min max length: the required maximum length reached.</li>"+
"<li>Min length change: the required change in length.</li>"+
"<li>Min Max DCTM: the required maximum Direction Correct Tip Movement.</li>"+
"<li>Min Max DCBM: the required maximum Direction Correct Base Movement.</li>"+
"<li>Max mean waviness: the maximum waviness calculated as the mean of 1-straightness over the life of the process.</li>"+
"</ul></p>"+
"<p style='font-size:8px;'>Filopodyan was developed at the Gurdon Institute by Richard Butler and Vasja Urbancic.<br><br>"+
"Copyright 2016 2017 2018, Richard Butler<br>"+
"Filopodyan is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by "+
"the Free Software Foundation, either version 3 of the License, or (at your option) any later version."+
"Filopodyan is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of "+
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.<br>"+
"You should have received a copy of the GNU General Public License along with Filopodyan.  If not, see http://www.gnu.org/licenses/.</p>"+
"</body></html>";

public boolean oked = false;
public boolean preview = false;
public boolean boundaryAnalysis = Prefs.get("Filopodyan.boundaryAnalysis", false);
public boolean tipPlot = Prefs.get("Filopodyan.tipPlot", false);
public boolean filoTable = Prefs.get("Filopodyan.filoTable", false);
public boolean coordTable = Prefs.get("Filopodyan.coordTable", false);
public boolean bodyTable = Prefs.get("Filopodyan.bodyTable", false);
public boolean kymographs = Prefs.get("Filopodyan.kymographs", false);
public boolean ccf = Prefs.get("Filopodyan.ccf", false);
public boolean basePlot = Prefs.get("Filopodyan.basePlot", false);
public boolean time = Prefs.get("Filopodyan.time", false);
public boolean adaptive = Prefs.get("Filopodyan.adaptive", false);
public boolean fit = Prefs.get("Filopodyan.fit", false);
public boolean verbose = Prefs.get("Filopodyan.verbose", false);
public boolean join = Prefs.get("Filopodyan.join", false);
public boolean showBackground = Prefs.get("Filopodyan.showBackground", false);
public boolean processProfile = Prefs.get("Filopodyan.processProfile", false);
public boolean advMode = Prefs.get("Filopodyan.advMode", false);
public int eds = (int)Prefs.get("Filopodyan.eds", 8);
public int backFrames = (int)Prefs.get("Filopodyan.backFrames", 3);
public int mapC = (int)Math.round(Prefs.get("Filopodyan.mapC", 1));
public int measureC = (int)Math.round(Prefs.get("Filopodyan.measureC", 2));
public double sigma = Prefs.get("Filopodyan.sigma", 3d);
public double weight = Prefs.get("Filopodyan.weight", 0.8d);
public String threshold = Prefs.get("Filopodyan.threshold", "Triangle");
public JFrame workFrame, helpFrame;
public int C;
private static final int BASIC = 0;
private static final int ADVANCED = 1;
public static final String[] methods = {"Triangle","Otsu","MaxEntropy","RenyiEntropy","Huang"};
private JLabel workLabel;

	/** Default constructor overriden by <code>BatchFilopodyan</code>*/
	public FilopodyanGui(){}

	/** Standard constuctor for non-batch mode GUI
	 * 
	 * 	@param parent	the parent Filopodyan PlugIn instance
	 * 	@param imp	the <code>ImagePlus</code> to be analysed
	 */
	public FilopodyanGui(Filopodyan_ parent,ImagePlus imp){
	try{
		this.parent = parent;
		this.target = imp;
		C = target.getNChannels();
		
		makeFrame();
		
		JPanel buttonPan = new JPanel();
		buttonPan.setLayout(new BoxLayout(buttonPan,BoxLayout.Y_AXIS));
		
		JPanel row2 = new JPanel();
		JButton previewButton = new JButton("Preview");
		previewButton.addActionListener(this);
		row2.add(previewButton);
		JButton logButton = new JButton("Log Settings");
		logButton.addActionListener(this);
		row2.add(logButton);
		buttonPan.add(row2);
		
		JPanel row1 = new JPanel();
		JButton okButton = new JButton("OK");
		okButton.addActionListener(this);
		row1.add(okButton);
		JButton cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(this);
		row1.add(cancelButton);
		JButton batchButton = new JButton("Batch Mode");
		batchButton.addActionListener(this);
		row1.add(batchButton);
		advList.add( batchButton );
		buttonPan.add(row1);

		add(buttonPan);
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
		
		this.log = FilopodyanLog.get();
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Create components used in both normal and batch modes without displaying anything. Completed and displayed by the single stack constructor or batch method
	 */
	public void makeFrame(){
		setTitle("Filopodyan");
		setLayout(new BoxLayout(this.getContentPane(),BoxLayout.Y_AXIS));
		setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("logo_icon.gif")));
		((JPanel)this.getContentPane()).setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
		
		String[] chans = new String[C];
		for(int c=1;c<=C;c++){
			chans[c-1] = ""+c;
		}

		advTick = new JToggleButton("advanced", advMode){
			private static final long serialVersionUID = 7574031909465176222L;
			private final Color selColour = new Color(255, 220, 255);
			private final Color unselColour = new Color(220, 255, 220);
			public final Font font = new Font(Font.SANS_SERIF, Font.BOLD, 12);
			
			@Override
			public void paintComponent(Graphics g){
				super.paintComponent(g);
				FontMetrics fm = g.getFontMetrics();
				Color colour = unselColour;
				String label = "basic";
				if(isSelected()){
					colour = selColour;
					label = "advanced";
				}
				g.setColor(colour);
				Rectangle rect = getBounds();
				g.fillRect(0, 0, rect.width, rect.height);
				g.setColor(Color.BLACK);
				g.setFont(font);
				g.drawString(label, (rect.width  - fm.stringWidth(label)) / 2 + 1,
                         			(rect.height + fm.getAscent()) / 2 - 1);
			}
		};
		advTick.setMargin(new Insets(0,0,0,0));
		advTick.addActionListener(this);

		JPanel advPan = new JPanel();
		advPan.add(advTick);
		
		JButton help = new JButton("?");
		help.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 16));
		help.setActionCommand("help");
		help.setPreferredSize(new Dimension(22,22));
		help.setMargin(new Insets(0,0,0,0));
		help.addActionListener(this);
		advPan.add(help);
		
		JPanel mapPanel = new JPanel();
		mapPanel.add(new JLabel("Map C"));
		mapCombo = new JComboBox<String>(chans);
		mapCombo.setSelectedItem(""+mapC);
		mapPanel.add(mapCombo);
		JPanel measurePanel = new JPanel();
		measurePanel.add(new JLabel("Measure C"));
		measureCombo = new JComboBox<String>(chans);
		measureCombo.setSelectedItem(""+measureC);
		measurePanel.add(measureCombo);
		add(makePanel(BASIC, mapPanel, measurePanel, advPan));
		
		thresholdCombo = new JComboBox<String>(methods);
		thresholdCombo.setSelectedItem(threshold);
		add( makePanel(BASIC, new JLabel("Threshold: ",JLabel.RIGHT), thresholdCombo) );
		adaptiveTick = new JCheckBox("",adaptive);
		add(makePanel(ADVANCED, new JLabel("Adaptive Thresholding", JLabel.RIGHT), adaptiveTick));
		fitTick = new JCheckBox("",fit);
		fitTick.addChangeListener(this);
		JPanel fitPanel = makePanel(ADVANCED, new JLabel("Fit tip to measure C", JLabel.RIGHT), fitTick);
		add(fitPanel);
		joinTick = new JCheckBox("",join);
		JPanel joinPanel = makePanel(ADVANCED, new JLabel("Join fragments",JLabel.RIGHT), joinTick);
		add(joinPanel);
		itField = new JTextField(""+eds,3);
		add( makePanel(BASIC, new JLabel("ED Iterations: ",JLabel.RIGHT), itField ) );
		backField = new JTextField(""+backFrames);
		add( makePanel(BASIC, new JLabel("Base Back Frames: ",JLabel.RIGHT), backField ) );
		sigmaField = new JTextField(""+sigma,3);
		add( makePanel(BASIC, new JLabel("LoG sigma: ",JLabel.RIGHT), sigmaField ) );
		
		
		filoTableTick = new JCheckBox("Filopodia Table",filoTable);
		filoTableTick.addChangeListener(this);
		coordTableTick = new JCheckBox("Coordinate Table",coordTable);
		coordTableTick.addChangeListener(this);
		add(makePanel(BASIC, filoTableTick, coordTableTick));
		bodyTableTick = new JCheckBox("Body Table",bodyTable);
		bodyTableTick.addChangeListener(this);
		add(makePanel(BASIC, bodyTableTick));
		
		processProfileTick = new JCheckBox("Process Profile Graphs",processProfile);
		processProfileTick.addChangeListener(this);
		add(makePanel(ADVANCED, processProfileTick));
		
		boundaryTick = new JCheckBox("Boundary Visualisation",boundaryAnalysis);
		boundaryTick.addChangeListener(this);
		kymographsTick = new JCheckBox("Kymographs",kymographs);
		kymographsTick.addChangeListener(this);
		add(makePanel(ADVANCED, boundaryTick));
		ccfTick = new JCheckBox("CCF",ccf);
		add(makePanel(ADVANCED, kymographsTick, ccfTick, (JComponent)Box.createHorizontalStrut(1)));
		timeTick = new JCheckBox("Show Time",time);
		verboseTick = new JCheckBox("Verbose Mode",verbose);
		add(makePanel(ADVANCED, timeTick, verboseTick));
		showBackgroundTick = new JCheckBox("Show Background Areas", showBackground);
		add( makePanel(ADVANCED, showBackgroundTick) );
		
		enableCheck();
		setAdvancedMode();
	}
	
	/** Disable <code>JCheckBox</code>es if the <code>JCheckBox</code>es for their prerequisite analysis are not selected.
	 */
	public void enableCheck(){
		boolean fitting = fitTick.isSelected();
		boundaryTick.setEnabled(!fitting);
		if(fitting&&boundaryTick.isSelected()){
			boundaryTick.setSelected(false);
		}
		
		boolean en = !fitting&&boundaryTick.isSelected();
		kymographsTick.setEnabled(en);
		ccfTick.setEnabled(en&&kymographsTick.isSelected());
		if(!en){
			kymographsTick.setSelected(false);
			ccfTick.setSelected(false);
		}
	}
	
	/**	Save parameters in <code>ij.Prefs</code>
	 */
	public void setPrefs(){
	try{
		Prefs.set("Filopodyan.mapC",mapC);
		Prefs.set("Filopodyan.measureC",measureC);
		Prefs.set("Filopodyan.threshold",threshold);
		Prefs.set("Filopodyan.eds",eds);
		Prefs.set("Filopodyan.backFrames",backFrames);
		Prefs.set("Filopodyan.sigma",sigma);
		Prefs.set("Filopodyan.boundaryAnalysis",boundaryAnalysis);
		Prefs.set("Filopodyan.processProfile", processProfile);
		Prefs.set("Filopodyan.filoTable",filoTable);
		Prefs.set("Filopodyan.coordTable",coordTable);
		Prefs.set("Filopodyan.bodyTable",bodyTable);
		Prefs.set("Filopodyan.kymographs",kymographs);
		Prefs.set("Filopodyan.ccf",ccf);
		Prefs.set("Filopodyan.time",time);
		Prefs.set("Filopodyan.adaptive",adaptive);
		Prefs.set("Filopodyan.fit",fit);
		Prefs.set("Filopodyan.verbose",verbose);
		Prefs.set("Filopodyan.join",join);
		Prefs.set("Filopodyan.showBackground",showBackground);
		Prefs.set("Filopodyan.advMode",advMode);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Get values from Components
	 * 
	 * @return	true - previous versions could return false, current implementation throws Exceptions for bad parameters instead.
	 */
	public boolean setFields(){
	try{
			threshold = (String)thresholdCombo.getSelectedItem();
			mapC = Integer.valueOf((String)mapCombo.getSelectedItem());
			measureC = Integer.valueOf((String)measureCombo.getSelectedItem());
			try{
				eds = Integer.parseInt(itField.getText());
			}catch(NumberFormatException nfe){IJ.error(itField.getText()+" is not an integer.");return false;}
			try{
				backFrames = Integer.parseInt(backField.getText());
			}catch(NumberFormatException nfe){IJ.error(itField.getText()+" is not an integer.");return false;}
			if(backFrames>0){backFrames = 0-backFrames;}
			try{
				sigma = Double.valueOf(sigmaField.getText());
			}catch(NumberFormatException nfe){IJ.error(sigmaField.getText()+" is not a number.");return false;}
			filoTable = filoTableTick.isSelected();
			coordTable = coordTableTick.isSelected();
			bodyTable = bodyTableTick.isSelected();
			fit = fitTick.isSelected();
			if(fit){
				boundaryAnalysis=false;
				kymographs=false;
				ccf=false;
			}
			else{
				boundaryAnalysis = boundaryTick.isSelected();
				kymographs = boundaryAnalysis&&kymographsTick.isSelected();
				ccf = boundaryAnalysis&&kymographs&&ccfTick.isSelected();
			}
			processProfile = processProfileTick.isSelected();
			time = timeTick.isSelected();
			verbose = verboseTick.isSelected();
			adaptive = adaptiveTick.isSelected();
			join = joinTick.isSelected();
			showBackground = showBackgroundTick.isSelected();
			
			if(!advMode){	//use defaults in basic mode but don't untick options
				time = false;
				verbose = false;
				fit = false;
				adaptive = false;
				join = false;
				showBackground = false;
				boundaryAnalysis = false;
				processProfile = false;
			}
			
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return true;
	}
	
	/** Set HTML text on the working information label
	 * 
	 * @param text	Text to be displayed, will be wrapped in &lt;html&gt;..&lt;/html&gt; so HTML tags can be included
	 */
	public void setLabel(String text){
		if(workLabel!=null&&text!=null){
			try{
			workLabel.setText("<html>"+text+"</html>");
			}catch(NullPointerException npe){}	//ignore strange Exception from View.setParent()
		}
	}
	
	/** Show the work frame to indicate that Filopodyan is running and provide an abort button.
	 */
	public void showWorkFrame(){
		if(workFrame==null){
			workFrame = new JFrame("Filopodyan");
			workFrame.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("logo_icon.gif")));
			workFrame.setLayout(new BorderLayout());
			workFrame.getContentPane().setBackground(Color.WHITE);
			java.net.URL url = this.getClass().getResource("working.gif");
			ImageIcon img = new ImageIcon(Toolkit.getDefaultToolkit().createImage(url));
			workLabel = new JLabel(img);
			workLabel.setFont(new Font("Sans-serif",Font.PLAIN,20));
			workFrame.add(workLabel,BorderLayout.CENTER);
			
			
			JPanel labelPanel = new JPanel();
			labelPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
			labelPanel.setBackground(Color.WHITE);
			workLabel = new JLabel("<html>...<br>...</html>",JLabel.LEFT);
			labelPanel.add(workLabel);
			workFrame.add(labelPanel,BorderLayout.NORTH);
	
			
			JButton abort = new JButton("abort");
			abort.addActionListener(new ActionListener(){
					@SuppressWarnings("deprecation")	//buggy. Uses deprecated stop() method in Thread - not thread safe but kills threads anyway
					public void actionPerformed(ActionEvent ae){
					try{
						if(verbose){IJ.log("Aborting Filopodyan...");}
						//adapted from Thread_Killer by Johannes Schindelin
						ThreadGroup group = Thread.currentThread().getThreadGroup();
						int activeCount = group.activeCount();
						Thread[] threads = new Thread[activeCount];
						group.enumerate(threads);
						for (Thread thread : threads) {
							String name = thread.getName();
							if(name.startsWith("Run$_")){	//if this is a SwingWorker or Executor pool Thread
								thread.stop();
							}
						}
						workFrame.dispose();
					}catch(Exception e){IJ.log("Aborting caused "+e.toString()+"\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
					finally{target.show();}
					}
			});
			workFrame.add(abort,BorderLayout.SOUTH);
			
			workFrame.pack();
			workFrame.setLocationRelativeTo(null);
			workFrame.setAlwaysOnTop(true);
		}
		workFrame.setVisible(true);
	}
	
	/** Display help in a <code>JFrame</code>
	 */
	public void showHelp(){
		if(helpFrame==null){
			helpFrame = new JFrame();
			helpFrame.setIconImage(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB_PRE));
			helpFrame.setPreferredSize(new Dimension(800, 800));
			helpFrame.setLayout(new BorderLayout());
			JPanel headerPan = new JPanel();
			headerPan.setBackground(Color.WHITE);
			JLabel header = new JLabel(	new ImageIcon(Toolkit.getDefaultToolkit().getImage(getClass().getResource("logo_borded_336x104.gif"))) );
			headerPan.add(header);
			helpFrame.add(BorderLayout.NORTH, headerPan);
			final JEditorPane textPane = new JEditorPane("text/html", helpText);
			textPane.setEditable(false);

			textPane.addHyperlinkListener(new HyperlinkListener() {
                @Override
                public void hyperlinkUpdate(HyperlinkEvent hle) {
                    if (HyperlinkEvent.EventType.ACTIVATED.equals(hle.getEventType())) {
                        try {
                        	Desktop.getDesktop().browse(hle.getURL().toURI());
                        } catch (Exception e) {
                        	JOptionPane.showMessageDialog(helpFrame, "Could not access "+hle.getURL()+"\n"+e.toString(), "Error", JOptionPane.ERROR_MESSAGE);
                        }
                    }
                }
            });

			textPane.setSelectionColor(Color.YELLOW);
			MouseAdapter helpMenuListen = new MouseAdapter(){
				public void mouseClicked(MouseEvent me){
				try{
					if(SwingUtilities.isRightMouseButton(me)){
						String search = textPane.getSelectedText();
						if(search==null){
							int offset = textPane.viewToModel(me.getPoint());
							int start = Utilities.getWordStart(textPane, offset);
							int end = Utilities.getWordEnd(textPane, offset);
							textPane.select(start, end);
							search = textPane.getSelectedText();
						}
						final String finalSearch = search;
						JPopupMenu rcMenu = new JPopupMenu();
						JMenuItem searchItem = new JMenuItem("Search Google for \""+finalSearch+"\"");
						searchItem.addActionListener(new ActionListener(){
							public void actionPerformed(ActionEvent ae){
								try{
									String param = finalSearch.replaceAll(" ", "%20").replaceAll("\\r?\\n", "");
									Desktop.getDesktop().browse( new URI( "https://www.google.co.uk/search?q="+param) );
								}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
							}
						});
						rcMenu.add(searchItem);
						rcMenu.pack();
						rcMenu.show((Component)textPane, me.getPoint().x, me.getPoint().y);
						
					}
				}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
				}
			};
			textPane.addMouseListener(helpMenuListen);
			JScrollPane scrollPane = new JScrollPane(textPane, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			helpFrame.add(BorderLayout.CENTER, scrollPane);
			helpFrame.pack();
			textPane.setCaretPosition(0);
		}
		helpFrame.setLocationRelativeTo(null);
		helpFrame.setVisible(true);
	}
	
	/** Send a string containing current detection settings to the <code>FilopodyanLog</code>
	 */
	public void logSettings(){
		setPrefs();
		String str ="Filopodyan settings "+(new Date().toString())+"\n"+
					"Map C: "+mapC+"\n"+
					"Measure C: "+measureC+"\n"+
					"Threshold: "+threshold+"\n"+
					"Adaptive thresholding: "+adaptive+"\n"+
					"Fit tip to measure C: "+fit+"\n"+
					"Join fragments: "+join+"\n"+
					"ED Iterations: "+eds+"\n"+
					"Base back frames: "+backFrames+"\n"+
					"LoG sigma: "+sigma+"\n";
		if(parent==null||parent.batch){
			log.print(str);
		}
		else{
			log.print(parent.imp.getTitle(), str);
		}
	}
	
	@Override
	public void dispose(){
		log.closeCheck();
		FilopodyanLog.get().dispose();
		super.dispose();
	}

	private JPanel makePanel(int type, JComponent... comps){
		if(advList==null){
			advList = new ArrayList<Component>();
		}
		JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(1,0,2,2));
		for(Component c : comps){
			panel.add(c);
		}
		if(type==ADVANCED){advList.add(panel);}
		return panel;
	}
	
	/** Set visibility of components based on whether advanced mode is enabled
	 */
	public void setAdvancedMode(){
		for(Component comp : advList){
			comp.setVisible(advMode);
		}
		pack();
	}	
	
	@Override
	public void actionPerformed(ActionEvent ae){
	try{
		String event = ae.getActionCommand();
		if(event=="Batch Mode"){
			parent.bgui = new BatchFilopodyan();
			((BatchFilopodyan)parent.bgui).createDialog();
			dispose();
		}
		else if(event=="OK"){
			if(!setFields()){return;}
			if(verbose){log.print(parent.imp.getTitle(), "Filopodyan OKed");}
			logSettings();
			if(!parent.gotImage()){return;}
			IJ.run(parent.imp, "Select None", "");
			
			showWorkFrame();
			
			setVisible(false);
			workGui = new SwingWorker<Object, Void>(){
				public Object doInBackground(){
					long t = System.currentTimeMillis();
					preview = false;
					setPrefs();
					FilopodyanProcessor fp = null;
					if(adaptive){
						fp = new ALTProcessor();
					}
					else{
						fp = new LoGProcessor();
					}
					parent.filopodia(false, fp);
					if(time){log.print(parent.imp.getTitle(), "Mapping took "+IJ.d2s((System.currentTimeMillis()-t)*0.001,2)+" seconds");}
					return null;
				}
				public void done(){
					workFrame.setVisible(false);
				}
			};
			workGui.execute();
		}
		else if(event=="Cancel"){
			if(verbose){log.print(parent.imp.getTitle(), "Filopodyan cancelled");}
			dispose();
		}
		else if(event=="Preview"){
			if(!parent.gotImage()){return;}
			preview = true;
			if(!setFields()){return;}
			if(verbose){log.print(parent.imp.getTitle(), "Filopodyan preview");}
			setPrefs();
			long t = System.currentTimeMillis();
			FilopodyanProcessor fp = null;
			if(adaptive){
				fp = new ALTProcessor();
			}
			else{
				fp = new LoGProcessor();
			}
			parent.filopodia(true, fp);
			if(time){log.print(parent.imp.getTitle(), "Filopodyan preview took "+IJ.d2s((System.currentTimeMillis()-t)*0.001,2)+" seconds");}
		}
		else if(event=="Log Settings"){
			logSettings();
		}
		else if(event.equals("advanced")){
			advMode = advTick.isSelected();
			Prefs.set("Filopodyan.advMode",advMode);
			setAdvancedMode();
		}
		else if(event.equals("help")){
			showHelp();
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	@Override
	public void stateChanged(ChangeEvent ce){
		enableCheck();
	}
	
}
