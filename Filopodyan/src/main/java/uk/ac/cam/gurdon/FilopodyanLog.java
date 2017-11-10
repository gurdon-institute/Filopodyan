package uk.ac.cam.gurdon;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.Document;

import ij.IJ;
import ij.Prefs;

/** Singleton class to show logs in a tabbed frame to keep separate logs for different images.
 *  Get the instance with the static get() method.
 *  
 *  @author Richard Butler
 */
public class FilopodyanLog{
public JFrame frame;
private JTabbedPane pane;
private String path = Prefs.get("FilopodyanLog.path", System.getProperty("user.home"));
private static final String DEFAULT_TAB = "log";
private static final FileNameExtensionFilter filter = new FileNameExtensionFilter("Filopodyan Log", "txt", "text");
private static FilopodyanLog instance;

	private FilopodyanLog(){
	try{
		ActionListener buttonListener = new ActionListener(){
			public void actionPerformed(ActionEvent ae){
			try{
				String event = ae.getActionCommand();
				int tabCount = pane.getTabCount();
				if(event.matches("Save.*")){
					if(event.matches("Save All.*?")){
						saveAllLogs();
					}
					else{
						saveLog(pane.getSelectedIndex());
					}
				}
				else if(event.equals("Close")){
					if(tabCount==0){
						IJ.error("No tab to close");
						return;
					}
					int closei = pane.getSelectedIndex();
					if( JOptionPane.showConfirmDialog(frame, "Save log "+pane.getTitleAt(closei)+"?", event, JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION ){
						saveLog(closei);
					}
					pane.remove(closei);
				}
			}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
			}
		};
		
		frame = new JFrame("Filopodyan Log");
		frame.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("logo_icon.gif")));
		frame.setLayout(new BorderLayout());
		JPanel controls = new JPanel();
		JButton saveButton = new JButton("Save Current Tab");
		saveButton.addActionListener(buttonListener);
		controls.add( saveButton );
		JButton saveAllButton = new JButton("Save All Tabs");
		saveAllButton.addActionListener(buttonListener);
		controls.add( saveAllButton );
		JButton closeButton = new JButton("Close");
		closeButton.addActionListener(buttonListener);
		controls.add( closeButton );
		frame.add(BorderLayout.NORTH, controls);
		pane = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
		frame.add(BorderLayout.CENTER, pane);
		frame.pack();
		frame.setSize(new Dimension(800,800));
		frame.setLocation(100,0);
		
		frame.addWindowListener(new WindowAdapter(){
				public void windowClosing(WindowEvent we){
					closeCheck();
				}
		});
		instance = this;
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Get the singleton FilopodyanLog instance
	 * 
	 * @return The FilopodyanLog instance, creating it if necessary
	 */
	public static FilopodyanLog get(){
	try{
		if(instance==null){ instance = new FilopodyanLog(); }
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
		return instance;
	}
	
	/** Get the JTabbedPane
	 * 
	 * @return	the JTabbedPane containing any logs
	 */
	public JTabbedPane getPane(){
		return pane;
	}
	
	private boolean properOS(){
		String name = System.getProperty("os.name").toLowerCase();
		if(name.matches(".*?mac.*?")){
			return false;
		}
		return true;
	}
	
	private void saveLog(int tabIndex){
		if(properOS()){
			saveLogJ(tabIndex);
		}
		else{
			iSaveLog(tabIndex);
		}
	}
	private void saveAllLogs(){
		if(properOS()){
			saveAllLogsJ();
		}
		else{
			iSaveAllLogs();
		}
	}
	
	private void saveAllLogsJ(){
	try{
		int tabCount = pane.getTabCount();
		JFileChooser fc = new JFileChooser(path);
		fc.setFileFilter(filter);
		fc.setDialogTitle("Save All Logs...");
		fc.setApproveButtonText("Save All");
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		if(fc.showOpenDialog(frame)==JFileChooser.APPROVE_OPTION){
			String path = fc.getSelectedFile().getAbsolutePath();
			for(int i=0;i<tabCount;i++){
				JScrollPane scroll = (JScrollPane) pane.getComponentAt(i);
				JTextPane textPane = (JTextPane) scroll.getViewport().getView();
				Document doc = textPane.getDocument();
				String logStr = doc.getText(0,doc.getLength());
				String sanTitle = pane.getTitleAt(i);
				sanTitle = sanTitle.replaceAll("\\.", "_");
				IJ.saveString(logStr, path+File.separator+sanTitle+".txt");
			}
			Prefs.set("FilopodyanLog.path", path);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	private void saveLogJ(int tabIndex){
	try{
		int tabCount = pane.getTabCount();
		if(tabIndex>tabCount-1||tabIndex<0){
			IJ.error("No log to save");
			return;
		}
		JFileChooser fc = new JFileChooser(path);
		fc.setFileFilter(filter);
		fc.setDialogTitle("Save Current Log...");
		fc.setApproveButtonText("Save");
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		String sanTitle = pane.getTitleAt(tabIndex);
		sanTitle = sanTitle.replaceAll("\\.", "_");
		fc.setSelectedFile(new File(path+File.separator+sanTitle+".txt"));
		if(fc.showOpenDialog(frame)==JFileChooser.APPROVE_OPTION){
			path = fc.getSelectedFile().getAbsolutePath();
			JScrollPane scroll = (JScrollPane) pane.getSelectedComponent();
			JTextPane textPane = (JTextPane) scroll.getViewport().getView();
			Document doc = textPane.getDocument();
			String logStr = doc.getText(0,doc.getLength());
			if(!path.endsWith(".txt")){
				path += ".txt";
			}
			IJ.saveString(logStr, path);
			Prefs.set("FilopodyanLog.path", path);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	private void iSaveLog(int tabIndex){
	try{
		FileDialog fd = new FileDialog(frame, "Save Current Log...");
		String sanTitle = pane.getTitleAt(tabIndex);
		sanTitle = sanTitle.replaceAll("\\.", "_");
		fd.setFile(path+File.separator+sanTitle+".txt");
		fd.setVisible(true);
		String dir = "";
		if((dir = fd.getDirectory()) != null){
			path = dir+File.separator+fd.getFile();
			JScrollPane scroll = (JScrollPane) pane.getSelectedComponent();
			JTextPane textPane = (JTextPane) scroll.getViewport().getView();
			Document doc = textPane.getDocument();
			String logStr = doc.getText(0,doc.getLength());
			if(!path.endsWith(".txt")){
				path += ".txt";
			}
			IJ.saveString(logStr, path);
			Prefs.set("FilopodyanLog.path", path);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	private void iSaveAllLogs(){
	try{
		int tabCount = pane.getTabCount();
		FileDialog fd = new FileDialog(frame, "Save All Logs...");
		fd.setDirectory(path);
		fd.setVisible(true);
		String dir = "";
		if((dir = fd.getDirectory()) != null){
			String path = dir;
			for(int i=0;i<tabCount;i++){
				JScrollPane scroll = (JScrollPane) pane.getComponentAt(i);
				JTextPane textPane = (JTextPane) scroll.getViewport().getView();
				Document doc = textPane.getDocument();
				String logStr = doc.getText(0,doc.getLength());
				String sanTitle = pane.getTitleAt(i);
				sanTitle = sanTitle.replaceAll("\\.", "_");
				IJ.saveString(logStr, path+File.separator+sanTitle+".txt");
			}
			Prefs.set("FilopodyanLog.path", path);
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Ask if logs should be saved if tabs exist
	 */
	public void closeCheck(){
	try{
		int tabCount = pane.getTabCount();
		if(tabCount>0){
			if(JOptionPane.showConfirmDialog(frame, "Save logs?", "Save logs?" ,JOptionPane.YES_NO_OPTION)==JOptionPane.YES_OPTION){
				saveAllLogs();
			}
		}
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Set the visibility of the JFrame
	 * 
	 * @param vis	frame visibility
	 */
	public void setVisible(boolean vis){
		frame.setVisible(vis);
	}
	
	/** Dispose the JFrame
	 */
	public void dispose(){
		frame.dispose();
		instance = null;
	}
	
	/** Add text to the default tab
	 * 
	 * @param txt	Text to be appended to the default tab log
	 */
	public void print(String txt){
		print(DEFAULT_TAB, txt);
	}
	
	/** Add text to a log, creates the log in a new tab if it doesn't already exist
	 * 
	 * @param title	The title of the log tab to append txt to
	 * @param txt	Text to be appended to the log
	 */
	public void print(String title, String txt){
	try{
		int tabi = pane.indexOfTab(title);
		if(tabi==-1){
			JTextPane textPane = new JTextPane();
			JScrollPane scroll = new JScrollPane(textPane);
			textPane.setEditable(false);
			pane.addTab(title, scroll);
		}
		tabi = pane.indexOfTab(title);
		JScrollPane scroll = (JScrollPane) pane.getComponentAt(tabi);
		JTextPane textPane = (JTextPane) scroll.getViewport().getView();
		Document doc = textPane.getDocument();
		int docn = doc.getLength();
		if(docn>0){ txt = "\n"+txt; }
		doc.insertString(docn, txt, null);
		textPane.setCaretPosition(doc.getLength());
		if(!frame.isVisible()){ frame.setVisible(true); }
	}catch(Exception e){IJ.log(e.toString()+"\n~~~~~\n"+Arrays.toString(e.getStackTrace()).replace(",","\n"));}
	}
	
	/** Add text to a log, creates the log in a new tab if it doesn't already exist
	 * 
	 * @param title	The title of the log tab to append txt to
	 * @param list	An ArrayList of String items to be appended to the log
	 */
	public void print(String title, ArrayList<String> list){
		for(String line:list){
			print(title, line);
		}
	}
	
}