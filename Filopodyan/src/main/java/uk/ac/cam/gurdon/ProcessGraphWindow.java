package uk.ac.cam.gurdon;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.nio.file.Paths;
import java.util.HashMap;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileNameExtensionFilter;

import ij.IJ;
import ij.ImagePlus;
import ij.Prefs;
import ij.gui.ImageCanvas;
import ij.measure.Calibration;
import ij.process.FloatProcessor;
import ij.process.ImageProcessor;
import ij.process.ImageStatistics;


public class ProcessGraphWindow extends JFrame {
	private static final long serialVersionUID = 108077997176300570L;
	private static int nCols = 3;	//number of columns in GridLayout
	private static Dimension display = Toolkit.getDefaultToolkit().getScreenSize();
	private static int size = (int) (display.width/((float)nCols+1));	//graph width in panel
	
	private Calibration distTimeCal;
	
	
	private class GraphPanel extends JPanel{
		private static final long serialVersionUID = -5506710653209153947L;

		private GraphPanel(int tracki, FloatProcessor graph){
			super(new BorderLayout());
			int width = graph.getWidth();
			int height = graph.getHeight();
			double scale = size/(float)width;
			
			ActionListener menuListener = new ActionListener(){
				@Override
				public void actionPerformed(ActionEvent ae){
					String cmd = ae.getActionCommand();
					if(cmd.equals("Save...")){
						String path = Prefs.get("FilopodyanProcessGraph.filepath", System.getProperty("user.home"))+"Track-"+tracki+".tif";
						JFileChooser chooser = new JFileChooser(path);
						chooser.setDialogTitle("Chooser...");
						chooser.setFileFilter(new FileNameExtensionFilter("TIFFs...", "tif", "tiff", "TIF", "TIFF"));
						chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
						int op = chooser.showDialog(null, "Save");
						if (op == JFileChooser.APPROVE_OPTION) {
							path = chooser.getSelectedFile().getAbsolutePath();
							Prefs.set("FilopodyanProcessGraph.filepath", path);

							ImagePlus imp = new ImagePlus("Track-"+tracki, graph);
							IJ.saveAsTiff(imp, path);
							imp.close();

							JOptionPane.showMessageDialog(null, "Saved Track "+tracki+" process graph as "+path, "Saved Graph", JOptionPane.PLAIN_MESSAGE);	
						}
					}
					else if(cmd.equals("Analyse")){
						analyse(graph);
					}
				}
			};
			
			MouseAdapter ma = new MouseAdapter(){
				JPopupMenu menu;
				
				@Override
				public void mouseClicked(MouseEvent me) {
					if(SwingUtilities.isLeftMouseButton(me) && me.getClickCount()>1){
						ImageProcessor ip = graph.duplicate();
						ImagePlus imp = new ImagePlus("Track "+tracki, ip);
						Calibration specialUnitCal = new Calibration();
						specialUnitCal.pixelWidth = distTimeCal.pixelWidth;
						specialUnitCal.pixelHeight = distTimeCal.pixelHeight;
						specialUnitCal.setUnit(distTimeCal.getUnit()+"*"+distTimeCal.getTimeUnit());
						imp.setCalibration(specialUnitCal);	//set calibration for x = distance, y = time with special 2D units
						imp.show();

						ImageCanvas ic = imp.getCanvas();
						ic.setSize((int)(width*scale), (int)(height*scale));
						ic.setSourceRect(new Rectangle(0, 0, (int)(width*scale), (int)(height*scale)));
						imp.getWindow().pack();
						ic.repaint();
					}
					else if(SwingUtilities.isRightMouseButton(me)){
						if(menu==null){
							menu = new JPopupMenu();
							JMenuItem save = new JMenuItem("Save...");
							save.addActionListener(menuListener);
							menu.add(save);
							/*JMenuItem analyse = new JMenuItem("Analyse");	//TODO
							analyse.addActionListener(menuListener);
							menu.add(analyse);*/
							menu.pack();
						}
						menu.show((Component) me.getSource(), me.getX(), me.getY());
					}
				}
				
				@Override
				public void mouseEntered(MouseEvent me){
					setBackground(Color.LIGHT_GRAY);
				}
				
				@Override
				public void mouseExited(MouseEvent me){
					setBackground(Color.WHITE);
				}
				
			};
			
			addMouseListener(ma);
			addMouseMotionListener(ma);
			
			FloatProcessor panelip = (FloatProcessor) graph.resize(size, (int)(height*(size/(float)width))); // scaled version of graph for display as BufferedImage
			ImageStatistics ipStats = panelip.getStatistics();
			panelip.setMinAndMax(ipStats.min, ipStats.max);	//set LUT to full range
			
			setBackground(Color.WHITE);
			JLabel imgLabel = new JLabel( new ImageIcon(panelip.getBufferedImage()), JLabel.CENTER );
			add(BorderLayout.CENTER, imgLabel);
			JLabel txtLabel = new JLabel( "Track "+tracki, JLabel.CENTER);
			add(BorderLayout.NORTH, txtLabel);
			
			add(BorderLayout.WEST, new AxisLabel(AxisLabel.Y, "Start", distTimeCal.getTimeUnit(), "End"));
			String unit = distTimeCal.getXUnit();
			add(BorderLayout.SOUTH, new AxisLabel(AxisLabel.X, "Base", unit, "Tip"));
			
			setBorder(BorderFactory.createLineBorder(Color.BLACK, 1));
		}
		
		@Override
		public void setBackground(Color colour){
			super.setBackground(colour);
			for(Component comp:getComponents()){
				comp.setBackground(colour);
			}
		}

	}
	
	
	private class AxisLabel extends JPanel{
		private static final long serialVersionUID = -4030434096478917244L;
		private final Font font = new Font(Font.SANS_SERIF, Font.PLAIN, 10);
		private static final int X = 0;
		private static final int Y = 1;
		
		private AxisLabel(int axis, String... strings){
			super();
			GridLayout layout = axis==X?new GridLayout(1,3):new GridLayout(3,1);
			setLayout(layout);
			setBackground(Color.WHITE);
			setBorder(BorderFactory.createEmptyBorder(2,2,2,2));
			for(String str:strings){
				JLabel label = new JLabel(str, JLabel.CENTER);
				label.setFont(font);
				add(label);
			}
		}
	}
	
	
	public ProcessGraphWindow(HashMap<Integer, FloatProcessor> profileGraphs, Calibration cal) {
		super("Filopodyan Process Graphs");
		this.distTimeCal = cal;
		
		setLayout(new BorderLayout());
		
		JPanel main = new JPanel(new GridLayout(0, nCols, 4, 4));
		main.setBackground(Color.WHITE);
		for(int tracki:profileGraphs.keySet()){
			GraphPanel gp = new GraphPanel(tracki, profileGraphs.get(tracki));
			main.add(gp);
		}
		
		JScrollPane sp = new JScrollPane(main);
		add(BorderLayout.CENTER, sp);
		
		ActionListener al = new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent ae){
				String cmd = ae.getActionCommand();
				if(cmd.equals("Save All...")){
					String path = Prefs.get("FilopodyanProcessGraph.path", System.getProperty("user.home"));
					JFileChooser chooser = new JFileChooser(path);
					chooser.setDialogTitle("Chooser...");
					chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
					int op = chooser.showDialog(null, "Save All");
					if (op == JFileChooser.APPROVE_OPTION) {
						path = chooser.getSelectedFile().getAbsolutePath();
						Prefs.set("FilopodyanProcessGraph.path", path);
						for(int tracki:profileGraphs.keySet()){
							ImagePlus imp = new ImagePlus("Track-"+tracki, profileGraphs.get(tracki));
							IJ.saveAsTiff(imp, Paths.get(path, "Track-"+tracki+".tif").toString());
							imp.close();
						}
						JOptionPane.showMessageDialog(null, "Saved "+profileGraphs.size()+" process graphs in "+path, "Saved Graphs", JOptionPane.PLAIN_MESSAGE);	
					}
				}
			}
		};
		
		JPanel controls = new JPanel();
		JButton saveAll = new JButton("Save All...");
		saveAll.addActionListener(al);
		controls.add(saveAll);
		add(BorderLayout.SOUTH, controls);
		
	}
	
	public void display() {
		pack();
		setLocationRelativeTo(null);
		if(getHeight()>display.height-100){
			setSize(getWidth(), display.height-100);
		}
		setVisible(true);
	}

	public void analyse(FloatProcessor graph){
		//TODO
	}
	
}
