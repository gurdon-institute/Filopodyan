import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import ij.IJ;
import ij.gui.Roi;
import ij.gui.ShapeRoi;
import ij.plugin.RoiEnlarger;

public class TrackEditor{
private JFrame gui;
private JPanel panel, holder;
private JScrollPane scroll;
private ActionListener listen;
private JRadioButton qdOffTick, qdTrackTick, qdObjectTick;
private JPopupMenu rcMenu;
private JDialog rangeDialog;
private JTextField startField, endField;
private static int width = 30;
private static int height = 20;
private static int gap = 5;
private int start,end,panW,panH;
private int scrollHorizontal,scrollVertical;
public ArrayList<ArrayList<Filopart>> filo;
private ArrayList<ArrayList<Filopart>> original;
private Point startPoint, dragPoint;
private boolean drag = false;
private Filopodyan_ parent;
private static final Font font = new Font(Font.SANS_SERIF,Font.PLAIN,12);
private PartNode startNode,dragNode;
private ArrayList<PartNode> nodeList;
private Timer clearTimer = new Timer();
private TimerTask task;
private ShapeRoi sel;
private int uniquei = 1;	//unique added index when keeping old duplicate track edit log pane, incremented each time
public boolean tracklog = true;	//toggle used to switch off logging when required
private boolean quickDeleteTrack = false;
private boolean quickDeleteObject = false;
private static final Stroke STROKE = new BasicStroke(5f);
private static final Dimension DISPLAY = Toolkit.getDefaultToolkit().getScreenSize();
private TrackEdits edits;

	private class PartNode extends JComponent{
		private static final long serialVersionUID = 573232345l;
		int index,x,y,t;
		Color colour = Color.GRAY;
		public PartNode(int t,int index){
			this.index = index;
			this.x = index*(width+gap);
			this.t = t;
			this.y = gap+(t*(height+gap));
			this.setSize(width,height);
			this.setFont(font);
			panW = Math.max(panW,x+width+gap);
			panH = Math.max(panH,y+height+gap);
		}
		public void paintComponent(Graphics g){
			super.paintComponent(g);
			g.setColor(colour);
			g.fillOval(0,0,width,height);
			g.setColor(Color.BLACK);
			FontMetrics metrics = g.getFontMetrics();
			g.drawString(""+index,(width-metrics.stringWidth(""+index))/2,(height/2)+5);
			setLocation(x,y);
		}
		public void setColour(Color col){
			this.colour = col;
		}
		public Dimension getPreferredSize(){
			return new Dimension(width,height);
		}
		public Dimension getMinimumSize(){
			return new Dimension(width,height);
		}
		public Dimension getMaximumSize(){
			return new Dimension(width,height);
		}
	}

	public TrackEditor(ArrayList<ArrayList<Filopart>> f,Filopodyan_ par){
	try{
		this.filo = f;
		this.parent = par;
		this.edits = new TrackEdits(this, parent.bgui.log, parent.title);
		
		JTabbedPane logPane = FilopodyanLog.get().getPane();
		int tabi = logPane.indexOfTab("Track Edits - "+parent.title);
		if( tabi != -1 ){
			if(JOptionPane.showConfirmDialog( parent.bgui, "Track Edit log Track Edits - "+parent.title+" already exists, create new?",
											 "New Log?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION )
			{
				String currentTitle = logPane.getTitleAt(tabi);
				while(logPane.indexOfTab(currentTitle+"-"+uniquei) != -1){	//increment until a title that does not exist is found
					uniquei++;
				}
				logPane.setTitleAt(tabi, currentTitle+"-"+uniquei);
				uniquei++;
			}
		}
		
		tracklog = false;
		sequentialise();
		tracklog = true;
		parent.update(filo);
		original = new ArrayList<ArrayList<Filopart>>();
		for(int t=0;t<filo.size();t++){
			ArrayList<Filopart> copy = new ArrayList<Filopart>();
			for(int p=0;p<filo.get(t).size();p++){
				copy.add(new Filopart(filo.get(t).get(p)));
			}
			original.add(copy);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void change(int from, int to, boolean min){
	try{
		if(from!=to){
			if(min&&from<to){	//make sure to is smaller than from
				int temp = to;
				to = from;
				from = temp;
			}
			ArrayList<ArrayList<Filopart>> old = new ArrayList<ArrayList<Filopart>>();
			for(int t=0;t<filo.size();t++){
				ArrayList<Filopart> copy = new ArrayList<Filopart>();
				for(int p=0;p<filo.get(t).size();p++){
					copy.add(new Filopart(filo.get(t).get(p)));
				}
				old.add(copy);
			}
			
			for(int t=0;t<filo.size();t++){
				Set<Integer> set = new HashSet<Integer>();
				for(int p=0;p<filo.get(t).size();p++){	//get existing indices at this t
					set.add(filo.get(t).get(p).index);
				}					
				for(int p=0;p<filo.get(t).size();p++){
					if(filo.get(t).get(p).index==from){
						if(set.contains(to)){
							IJ.error("Track Index Error", "Changing from "+from+" to "+to+" would make a duplicate index at T"+(t+1));
							filo = old;	//reset to unedited version
							return;
						}
						filo.get(t).get(p).index=to;
					}
				}
			}
			if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Index "+from+" changed to "+to);}
			if(tracklog){ edits.add(TrackEdits.Op.CHANGE, from, to, min); }
		}
		from = -1;
		to = -1;
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void update(){
		if(scroll!=null){	//if the panel has already been constructed
			gui.remove(scroll);
			scroll = makePanel();
			gui.add(scroll);
			gui.pack();
		}
		parent.update(filo);
		if(tracklog){ edits.add(TrackEdits.Op.UPDATE); }
	}
	
	public void delete(int trackI, int t1, int t2){
	try{
		if(t1<1){t1=1;}
		if(t2<t1){t2=t1;}
		if(t2>filo.size()){t2=filo.size();}
		for(int t=t1-1;t<t2;t++){
			for(int p=0;p<filo.get(t).size();p++){
				if(filo.get(t).get(p).index==trackI){
					filo.get(t).remove(p);
				}
			}
		}
		if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Deleted "+t1+" to "+t2+" from track "+trackI);}
		if(tracklog){edits.add(TrackEdits.Op.DELETE, trackI, t1, t2);}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void delete(PartNode del,boolean wholeTrack,boolean confirm){
	try{
		if(!wholeTrack){
			if(confirm){
				if(JOptionPane.showConfirmDialog(gui,"Delete single object in track "+del.index+"?","Delete?",JOptionPane.YES_NO_OPTION)==JOptionPane.NO_OPTION){
					return;
				}
			}
			outer:
			for(int t=0;t<filo.size();t++){
				for(int p=0;p<filo.get(t).size();p++){
					if(filo.get(t).get(p).index==del.index&&t==del.t){
						filo.get(t).remove(p);
						if(tracklog){edits.add(TrackEdits.Op.DELETE, del.index, t+1, t+1);}
						if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Deleted object from track "+del.index+" at T"+(t+1));}
						break outer;
					}
				}
			}
			
		}
		else{
			if(confirm){
				if(JOptionPane.showConfirmDialog(gui,"Delete track "+del.index+"?","Delete Whole Track?",JOptionPane.YES_NO_OPTION)==JOptionPane.NO_OPTION){
					return;
				}
			}
			for(int t=0;t<filo.size();t++){
				for(int p=0;p<filo.get(t).size();p++){
					if(filo.get(t).get(p).index==del.index){
						filo.get(t).remove(p);
						if(tracklog){edits.add(TrackEdits.Op.DELETE, del.index, t+1, t+1);}
					}
				}
			}
			if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Deleted track "+del.index);}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	private class KLabel extends JLabel{
		private static final long serialVersionUID = 56473829342l;
		public KLabel(String str,int align){
			setText(str);
			setHorizontalAlignment(align);
		}
		public Dimension getPreferredSize(){
			return new Dimension(width+gap,height+gap);
		}
		public Dimension getMinimumSize(){
			return new Dimension(width+gap,height+gap);
		}
		public Dimension getMaximumSize(){
			return new Dimension(width+gap,height+gap);
		}
	}
	
	public void sequentialise(){
	try{
		if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Sequentialising track indices");}
		ArrayList<Integer> indices = new ArrayList<Integer>();
		for(int t=0;t<filo.size();t++){
			for(int p=0;p<filo.get(t).size();p++){
				int ind = filo.get(t).get(p).index;
				if(!indices.contains(ind)){
					indices.add(ind);
				}
			}
		}
		Collections.sort(indices);
		for(int i=0;i<indices.size();i++){
			if(indices.get(i) != i){
				change( indices.get(i), i, true );
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void restore(){
	try{
		//reset filo and remake original lists with FiloPart copying constructor
		filo = original;
		original = new ArrayList<ArrayList<Filopart>>();
		for(int t=0;t<filo.size();t++){
			ArrayList<Filopart> copy = new ArrayList<Filopart>();
			for(int p=0;p<filo.get(t).size();p++){
				copy.add(new Filopart(filo.get(t).get(p)));
			}
			original.add(copy);
		}

		parent.update(filo);
		gui.remove(scroll);
		scroll = makePanel();
		gui.add(scroll);
		gui.pack();
		scroll.repaint();
		if(tracklog){edits.add(TrackEdits.Op.RESTORE);}
		if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Restored original tracks");}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	private void setIndex(PartNode part){
	try{
		int from = part.index;
		String toStr = JOptionPane.showInputDialog(gui,"Change index "+from+" to:","Change Index",JOptionPane.PLAIN_MESSAGE);
		if(toStr==null||toStr.length()==0){return;}
		int to = -1;
		try{
			to = Integer.valueOf(toStr);
		}catch(NumberFormatException nfe){IJ.error(toStr+" is not an integer");return;}
		change(from,to,false);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	public void run(){
	try{
		gui = new JFrame("Edit Tracks");
		gui.setLayout(new BorderLayout());
		gui.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("logo_icon.gif")));
		
		listen = new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				String event = ae.getActionCommand();
				
				if(event=="Delete"){
					delete(startNode,false,true);
				}
				else if(event=="Delete Track"){
					delete(startNode,true,true);
				}
				else if(event=="Delete Range"){
					if(rangeDialog==null){
						rangeDialog = new JDialog(gui, "Delete from Track "+startNode.index, true);
						rangeDialog.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
						rangeDialog.setLocationRelativeTo(null);
						rangeDialog.add(new JLabel("T "));
						startField = new JTextField("1", 2);
						rangeDialog.add(startField);
						rangeDialog.add(new JLabel(" to "));
						endField = new JTextField(""+filo.size(), 2);
						rangeDialog.add(endField);
						JButton button = new JButton("Delete");
						button.addActionListener(new ActionListener(){
								public void actionPerformed(ActionEvent ae){
									int start = -1;
									int end = -1;
									try{
										start = Integer.valueOf(startField.getText());
										end = Integer.valueOf(endField.getText());
									}catch(NumberFormatException nfe){IJ.error("Please enter integers for timepoint range");}
									delete(startNode.index, start, end);
									rangeDialog.setVisible(false);
								}
						});
						rangeDialog.add(button);
						rangeDialog.pack();
					}
					rangeDialog.setVisible(true);
				}
				else if(event=="Set Track Index"){
					setIndex(startNode);
				}
				if(startNode!=null){startNode.setColour(Color.GRAY);}
				if(dragNode!=null){dragNode.setColour(Color.GRAY);}
				update();
				panel.repaint();
			}
		};
		
		scroll = makePanel();
		gui.add(scroll,BorderLayout.CENTER);
		
		JPanel buttonPan = new JPanel(new FlowLayout(FlowLayout.CENTER));
		buttonPan.setBackground(Color.WHITE);
		JButton done = new JButton("Done");
		done.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent ae){
					gui.dispose();
					parent.trackEdited(filo);
				}
		});
		buttonPan.add(done);
		JButton sequentialise = new JButton("Make sequential");
		sequentialise.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent ae){
					sequentialise();
					update();
				}
		});
		buttonPan.add(sequentialise);
		
		
		JButton restore = new JButton("Restore");
		restore.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent ae){
					restore();
					update();
				}
		});
		buttonPan.add(restore);
		
		JButton loadEdits = new JButton("Load Edits");
		loadEdits.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent ae){
					edits.load();
					edits.apply();
				}
		});
		buttonPan.add(loadEdits);
		
		ButtonGroup deleteGroup = new ButtonGroup();
		ActionListener qdListen = new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				quickDeleteTrack = qdTrackTick.isSelected();
				quickDeleteObject = qdObjectTick.isSelected();
				panel.setBackground(quickDeleteTrack?Color.RED:quickDeleteObject?Color.ORANGE:Color.WHITE);
			}
		};
		buttonPan.add(new JLabel("Quick Delete:",JLabel.RIGHT));
		qdOffTick = new JRadioButton("off",false);	qdOffTick.setBackground(Color.WHITE);
		deleteGroup.add(qdOffTick);
		buttonPan.add(qdOffTick); qdOffTick.addActionListener(qdListen);
		qdObjectTick = new JRadioButton("objects",false);	qdObjectTick.setBackground(Color.WHITE);
		deleteGroup.add(qdObjectTick);
		buttonPan.add(qdObjectTick); qdObjectTick.addActionListener(qdListen);
		qdTrackTick = new JRadioButton("tracks",false);	qdTrackTick.setBackground(Color.WHITE);
		deleteGroup.add(qdTrackTick);
		buttonPan.add(qdTrackTick); qdTrackTick.addActionListener(qdListen);
		qdOffTick.setSelected(true);
		gui.add(buttonPan,BorderLayout.SOUTH);
		
		gui.pack();
		gui.setLocationRelativeTo(null);
		gui.setVisible(true);
		parent.setImageVisible(true);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	

	private JScrollPane makePanel(){
		scrollHorizontal = 0;
		scrollVertical = 0;
		int maxW = 0;
		int maxH = 0;
		try{
			if(scroll!=null){
				scrollHorizontal = scroll.getHorizontalScrollBar().getValue();
				scrollVertical = scroll.getVerticalScrollBar().getValue();
				maxW = scroll.getWidth();
				maxH = scroll.getHeight();
			}
			else{
				maxW = DISPLAY.width-600+width+gap;
				maxH = DISPLAY.height-100+height+gap;
			}
		}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		
		scroll = new JScrollPane();
	try{
		holder = new JPanel();
		holder.setBackground(Color.WHITE);
		
		JPanel time = new JPanel();
		time.setLayout(new BoxLayout(time,BoxLayout.Y_AXIS));
		time.setBackground(Color.WHITE);
		for(int t=0;t<filo.size();t++){
			KLabel label = new KLabel(""+(t+1),SwingConstants.CENTER);
			label.setBorder(BorderFactory.createLineBorder(Color.BLACK,1));
			time.add(label);
		}
		gui.add(time,BorderLayout.WEST);
		
		panel = new JPanel(){
			private static final long serialVersionUID = -3352592112566814511L;
			public void paintComponent(Graphics g){
				super.paintComponent(g);
				Graphics2D g2d = (Graphics2D)g;
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g2d.setStroke(STROKE);
				if(drag&&startPoint!=null&&dragPoint!=null){
					g2d.setColor(Color.RED);
					g2d.drawLine(startPoint.x,startPoint.y,dragPoint.x,dragPoint.y);
				}
			}
		};
		panel.setLayout(null);
		panel.setBackground(quickDeleteTrack?Color.RED:quickDeleteObject?Color.ORANGE:Color.WHITE);
		panW = 0; panH = 0;
		nodeList = new ArrayList<PartNode>();
		if(parent.bgui.verbose){FilopodyanLog.get().print(parent.imp.getTitle(), "Constructing Track Editor for "+filo.size()+" frames");}
		for(int t=0;t<filo.size();t++){
			for(int p=0;p<filo.get(t).size();p++){
				PartNode b = new PartNode(t,filo.get(t).get(p).index);
				nodeList.add(b);
				panel.add(b);
			}
		}
		
		MouseAdapter ma = new MouseAdapter(){
			public void mousePressed(MouseEvent me){
			try{
				Component comp = panel.getComponentAt(me.getPoint());
				if(!(comp instanceof PartNode)){
					if(startNode!=null){startNode.setColour(Color.GRAY);}
					if(dragNode!=null){dragNode.setColour(Color.GRAY);}
					startPoint = null;	dragPoint = null;
					drag = false;
					panel.repaint();
					return;
				}
				if(SwingUtilities.isLeftMouseButton(me)){
					startNode = ((PartNode)panel.getComponentAt(me.getPoint()));
					startNode.setColour(Color.CYAN);
					start = startNode.index;
					dragNode = null;
					end = -1;
					startPoint = me.getPoint();
					panel.repaint();
				}
			}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
			}
			public void mouseReleased(MouseEvent me){
			try{
				if(startNode!=null){startNode.setColour(Color.GRAY);}
				if(dragNode!=null){dragNode.setColour(Color.GRAY);}
				drag = false;
				Component comp = panel.getComponentAt(me.getPoint());
				if(!(comp instanceof PartNode)){panel.repaint();return;}
				if(SwingUtilities.isLeftMouseButton(me)){
					end = ((PartNode)panel.getComponentAt(me.getPoint())).index;
					if(start!=end){
						if(JOptionPane.showConfirmDialog(gui,"Join tracks "+start+" and "+end+"?","Make Link?",JOptionPane.YES_NO_OPTION)==JOptionPane.OK_OPTION){
							change(Math.min(start,end),Math.max(start,end),true);
							update();
						}
					}
				}
				panel.repaint();
			}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
			}
			public void mouseDragged(MouseEvent me){
			try{
				if(SwingUtilities.isLeftMouseButton(me)){
					Component comp = panel.getComponentAt(me.getPoint());
					if(!(comp instanceof PartNode)){return;}
					if(dragNode!=null){dragNode.setColour(Color.GRAY);}
					dragNode = (PartNode)comp;
					if(startNode!=null){startNode.setColour(Color.CYAN);}
					dragNode.setColour(Color.CYAN);
					dragPoint = me.getPoint();
					drag = true;
					panel.repaint();
					((JComponent)comp.getParent()).scrollRectToVisible(comp.getBounds());	//get JViewport as JComponent, scroll to comp
				}
			}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
			}
			public void mouseExited(MouseEvent me){
			try{
				if(rcMenu!=null&&rcMenu.isVisible()){return;}
				if(startNode!=null){startNode.setColour(Color.GRAY);}
				if(dragNode!=null){dragNode.setColour(Color.GRAY);}
				drag = false;
				panel.repaint();
			}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
			}
			public void mouseClicked(MouseEvent me){
			try{
				Component comp = panel.getComponentAt(me.getPoint());
				if(!(comp instanceof PartNode)){return;}
				if(SwingUtilities.isLeftMouseButton(me)){
					startNode = (PartNode)comp;
					for(int p=0;p<filo.get(startNode.t).size();p++){
						if(filo.get(startNode.t).get(p).index==startNode.index){
							Filopart target = filo.get(startNode.t).get(p);
							
							sel = new ShapeRoi(target.roi);
							sel.setStrokeColor(Color.YELLOW);
							
							startNode.setColour(Color.YELLOW);
							parent.imp.setPosition(0,4,startNode.t+1);
							parent.imp.setRoi(sel);
							task = new TimerTask(){
								int n = 1;
								public void run(){
								try{
									if(n>10){
										parent.imp.setRoi(new Roi(0,0,0,0));
										startNode.setColour(Color.GRAY);
										panel.repaint();
										this.cancel();
									}
									else{
										sel = new ShapeRoi(RoiEnlarger.enlarge(sel, 0d+n));
										sel.setStrokeColor(Color.YELLOW);
										parent.imp.setPosition(0,4,startNode.t+1);
										parent.imp.setRoi(sel);
										n++;
									}
								}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
								}
							};
							clearTimer.purge();
							clearTimer.scheduleAtFixedRate(task,20l,20l);
							break;
						}
					}
				}
				else if(SwingUtilities.isRightMouseButton(me)){
					startNode = ((PartNode)comp);
					if(quickDeleteTrack){
						delete(startNode,true,false);
						update();
						return;
					}
					else if(quickDeleteObject){
						delete(startNode,false,false);
						update();
						return;
					}
					if(rcMenu==null){
						rcMenu = new JPopupMenu();
						JMenuItem number = new JMenuItem(""+startNode.index);
						number.setEnabled(false);
						rcMenu.add(number);
						JMenuItem single = new JMenuItem("Delete");
						single.addActionListener(listen);
						rcMenu.add(single);
						JMenuItem track = new JMenuItem("Delete Track");
						track.addActionListener(listen);
						rcMenu.add(track);
						JMenuItem range = new JMenuItem("Delete Range");
						range.addActionListener(listen);
						rcMenu.add(range);
						JMenuItem index = new JMenuItem("Set Track Index");
						index.addActionListener(listen);
						rcMenu.add(index);
						rcMenu.pack();
					}
					rcMenu.show((Component)startNode,width/2,height/2);
					startNode.setColour(Color.YELLOW);
				}
			}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
			}
		};
		panel.addMouseListener(ma);
		panel.addMouseMotionListener(ma);
		panel.setPreferredSize(new Dimension(panW,panH));
		holder.add(time);
		holder.add(panel);
		scroll = new JScrollPane(holder);
		scroll.getVerticalScrollBar().setUnitIncrement(height);
		
		int scrollW = panW+((width+gap)*2);
		int scrollH = panH+height;
		scroll.setPreferredSize(new Dimension( (scrollW>maxW)?maxW:scrollW , (scrollH>maxH)?maxH:scrollH ));
		/* SwingUtilities.invokeLater(new Runnable(){	//reset the scrollbar position off the EDT - broken
				public void run(){
					scroll.getHorizontalScrollBar().setValue(scrollHorizontal);
					scroll.getVerticalScrollBar().setValue(scrollVertical);
				}
		}); */
		TimerTask task = new TimerTask(){	//invokeLater leaves the pane blank - a Timer is stupid but works
			public void run(){
				scroll.getHorizontalScrollBar().setValue(scrollHorizontal);
				scroll.getVerticalScrollBar().setValue(scrollVertical);
			}
		};
		Timer timer = new Timer();
		timer.schedule(task,200L);
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return scroll;
	}
	
}
