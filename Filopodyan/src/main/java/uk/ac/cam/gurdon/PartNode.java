package uk.ac.cam.gurdon;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.JComponent;

/** <code>JComponent</code> to display an object in a <code>TrackEditor</code>.
 * @see TrackEditor
 *  
 * @author Richard Butler
 */
public class PartNode extends JComponent{

	private TrackEditor trackEditor;
	private static final long serialVersionUID = 573232345l;
	int index,x,y,t;
	Color colour = Color.GRAY;
	
	/** Create a <code>PartNode</code> with specified time and track indices
	 * 
	 * @param t	the time index
	 * @param index	the track index
	 * @param trackEditor the TrackEditor that will display this <code>PartNode</code>
	 */
	public PartNode(TrackEditor trackEditor, int t,int index){
		this.trackEditor = trackEditor;
		this.index = index;
		this.x = index*(TrackEditor.width+TrackEditor.gap);
		this.t = t;
		this.y = TrackEditor.gap+(t*(TrackEditor.height+TrackEditor.gap));
		this.setSize(TrackEditor.width,TrackEditor.height);
		this.setFont(TrackEditor.font);
		this.trackEditor.panW = Math.max(this.trackEditor.panW,x+TrackEditor.width+TrackEditor.gap);
		this.trackEditor.panH = Math.max(this.trackEditor.panH,y+TrackEditor.height+TrackEditor.gap);
	}
	@Override
	public void paintComponent(Graphics g){
		super.paintComponent(g);
		g.setColor(colour);
		g.fillOval(0,0,TrackEditor.width,TrackEditor.height);
		g.setColor(Color.BLACK);
		FontMetrics metrics = g.getFontMetrics();
		g.drawString(""+index,(TrackEditor.width-metrics.stringWidth(""+index))/2,(TrackEditor.height/2)+5);
		setLocation(x,y);
	}
	
	/** Sets the fill colour.
	 * 
	 * @param col	The fill <code>Color</code>.
	 */
	public void setColour(Color col){
		this.colour = col;
	}
	@Override
	public Dimension getPreferredSize(){
		return new Dimension(TrackEditor.width, TrackEditor.height);
	}
	@Override
	public Dimension getMinimumSize(){
		return new Dimension(TrackEditor.width, TrackEditor.height);
	}
	@Override
	public Dimension getMaximumSize(){
		return new Dimension(TrackEditor.width, TrackEditor.height);
	}
}