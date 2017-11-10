package uk.ac.cam.gurdon;
import ij.*;

import java.io.File;
import java.lang.Exception;
import java.util.ArrayList;

import java.util.Arrays;

import java.util.Iterator;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;


public class TrackEdits{

private TrackEditor editor;
private FilopodyanLog log;
private ArrayList<String> list;
private String imageTitle;
private String loadPath = Prefs.get("Bounder.loadPath",System.getProperty("user.home"));

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

	public TrackEdits(TrackEditor editor, FilopodyanLog log, String imageTitle){
		this.editor = editor;
		this.log = log;
		this.imageTitle = imageTitle;
		this.list = new ArrayList<String>();
		log.print(imageTitle, "Track Edits-");
	}
	
	public void add(Op edit){
		String op = ""+edit.getInt()+", "+edit.getString();
		list.add(op);
		log.print(imageTitle,edit.getString());
		log.print("Track Edits - "+imageTitle,op);
	}
	
	public void add(Op edit, int from, int to, boolean min){
		String op = edit.getInt()+","+from+","+to+","+(min?"1":"0")+", "+edit.getString(from, to);
		list.add(op);
		log.print(imageTitle,edit.getString(from, to));
		log.print("Track Edits - "+imageTitle,op);
	}
	
	public void add(Op edit, int track, int t1, int t2){
		String op = edit.getInt()+","+track+","+t1+","+t2+", "+edit.getString(track, t1, t2);
		list.add(op);
		log.print(imageTitle,edit.getString(track, t1, t2));	//readable form only
		log.print("Track Edits - "+imageTitle,op);		//applyable form including readable form
	}
	
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