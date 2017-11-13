package uk.ac.cam.gurdon;
import ij.*;

import java.io.File;
import java.lang.Exception;
import java.util.ArrayList;

import java.util.Arrays;

import java.util.Iterator;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

/** Store edits made to tracks in <code>TrackEditor</code> and allows sequences of edits to be saved and loaded.
 * 
 * @author Richard Butler
 */
public class TrackEdits{

private TrackEditor editor;
private FilopodyanLog log;
private ArrayList<String> list;
private String imageTitle;
private String loadPath = Prefs.get("Bounder.loadPath",System.getProperty("user.home"));

	/** edit operations: change, delete, restore and update
	 *  contains methods for convenient generation of human-readable Strings
	 */
	public static enum Op{
		CHANGE(0), DELETE(1), RESTORE(2), UPDATE(3);
		int i;
		
		Op(int i){
			this.i = i;
		}
		
		public int getInt(){
			return i;
		}
		
		public String getString() throws IllegalArgumentException{
			if(i==2){
				return "Restored original tracks";
			}
			else if(i==3){
				return "Updated tracks";
			}
			throw new IllegalArgumentException("Wrong number of arguments for getString in Op "+i);
		}
		
		public String getString(int track, int t1, int t2){
			if(i==1){
				return "Deleted T"+t1+" to T"+t2+" from track "+track;
			}
			throw new IllegalArgumentException("Wrong number of arguments for getString in Op "+i);
		}
		
		public String getString(int from, int to){
			if(i==0){
				return "Index "+from+" changed to "+to;
			}
			throw new IllegalArgumentException("Wrong number of arguments for getString in Op "+i);
		}
		
	}

	/** Create a new sequence of edits for an editor.
	 * 
	 *  @param editor	The <code>TrackEditor</code> used to get and apply edits
	 *  @param log	The <code>FilopodyanLog</code> for logging edits
	 *  @param imageTitle	The title of the <code>FilopodyanLog</code> tab to use
	 */
	public TrackEdits(TrackEditor editor, FilopodyanLog log, String imageTitle){
		this.editor = editor;
		this.log = log;
		this.imageTitle = imageTitle;
		this.list = new ArrayList<String>();
		log.print(imageTitle, "Track Edits-");
	}
	
	/** Add an edit to the list of operations and log it as comma delimited ints to save/apply and in human-readable form
	 * 
	 * @param edit	The <code>Op</code> representing the type of edit
	 */
	public void add(Op edit){
		String op = ""+edit.getInt()+", "+edit.getString();
		list.add(op);
		log.print(imageTitle,edit.getString());
		log.print("Track Edits - "+imageTitle,op);
	}
	
	/** Add an edit to the list of operations and log it as comma delimited ints to save/apply and in human-readable form
	 * 
	 * @param edit	The <code>Op</code> representing the type of edit
	 * @param from	The index changed from
	 * @param to	The index changed to
	 * @param min	true if the smaller index should be assigned, false to allow a larger index to be set
	 */
	public void add(Op edit, int from, int to, boolean min){
		String op = edit.getInt()+","+from+","+to+","+(min?"1":"0")+", "+edit.getString(from, to);
		list.add(op);
		log.print(imageTitle,edit.getString(from, to));
		log.print("Track Edits - "+imageTitle,op);
	}
	
	/** Add an edit to the list of operations and log it as comma delimited ints to save/apply and in human-readable form
	 * 
	 * @param edit	The <code>Op</code> representing the type of edit
	 * @param track	The index of the edited track
	 * @param t1	The first timepoint edited
	 * @param t2	The last timepoint edited
	 */
	public void add(Op edit, int track, int t1, int t2){
		String op = edit.getInt()+","+track+","+t1+","+t2+", "+edit.getString(track, t1, t2);
		list.add(op);
		log.print(imageTitle,edit.getString(track, t1, t2));	//readable form only
		log.print("Track Edits - "+imageTitle,op);		//applyable form including readable form
	}
	
	/** Load a sequence of edits from a text file
	 */
	public void load(){
	try{
		list = new ArrayList<String>();
		String str = "";
		JFileChooser fc = new JFileChooser(loadPath);
		FileNameExtensionFilter filter = new FileNameExtensionFilter("Track Edits", "txt", "text");
		fc.setFileFilter(filter);
		fc.setDialogTitle("Load Edits...");
		fc.setApproveButtonText("Load");
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fc.setSelectedFile(new File(loadPath));
		if(fc.showOpenDialog(null)==JFileChooser.APPROVE_OPTION){
			loadPath = fc.getSelectedFile().getAbsolutePath();
			str = IJ.openAsString(loadPath);
			String[] lines = str.split("\n");
			for(String line : lines){
				list.add(line);
			}
			Prefs.set("Bounder.loadPath",loadPath);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}

	/** Apply the current list of edits to the tracks in the <code>TrackEditor</code>
	 */
	public void apply(){
	try{
		editor.tracklog = false;
		for(Iterator<String> lines = list.iterator(); lines.hasNext();){
			String line = lines.next();
			String[] params = line.split(",");
			int edit = Integer.valueOf(params[0]);
			if(edit==Op.CHANGE.getInt()){
				int from = Integer.valueOf(params[1]);
				int to = Integer.valueOf(params[2]);
				boolean min = Boolean.valueOf(params[3]);
				editor.change(from, to, min);
			}
			else if(edit==Op.DELETE.getInt()){
				int index = Integer.valueOf(params[1]);
				int t1 = Integer.valueOf(params[2]);
				int t2 = Integer.valueOf(params[3]);
				editor.delete(index, t1, t2);
			}
			else if(edit==Op.RESTORE.getInt()){
				editor.restore();
			}
			else if(edit==Op.UPDATE.getInt()){
				editor.update();
			}
			log.print(imageTitle,params[params.length-1]);	//log the text description
		}
		editor.tracklog = true;
		log.print("Track Edits - "+imageTitle,list); //log the applied edits. Done here to avoid ConcurrentModificationException from list
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
}