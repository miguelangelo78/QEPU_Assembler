import java.awt.HeadlessException;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import javax.swing.JOptionPane;

public class qassembler {
	
	private static final String FILESOURCE_FORMAT=".qep",
						 		FILEBINARY_FORMAT=".bin";
	
	public static void messagebox(String title,String msg,int message_type,boolean close){
		JOptionPane.showMessageDialog(null,msg,title,message_type);
		if(close) System.exit(1);
	}
	
	public static String readFile(String filepath) throws FileNotFoundException{
		BufferedReader br=new BufferedReader(new FileReader(filepath));
		String text="";
		String line="";
		try {
			while ((line= br.readLine()) != null)
				text+=line+"\n";
			br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return text;
	}
	
	public static void main(String[] args) {
		
		try {
			String n="test.qep";
			JOptionPane.showMessageDialog(null,new QEPUAssembler(n.replace(FILESOURCE_FORMAT,FILEBINARY_FORMAT)).assemble(readFile(n)), "Assembled successfully", JOptionPane.INFORMATION_MESSAGE);
		} catch (HeadlessException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		
		/*if(args.length!=1)
			messagebox("Quantum Assembler Error","Usage: java -jar qassembler.jar \"C:\\....\\filename"+filesource_format+"\"",JOptionPane.ERROR_MESSAGE,true);
		if(!args[0].contains(filesource_format)) messagebox("Quantum Assembler Error","The file format is wrong. Use '"+filesource_format+"'",JOptionPane.ERROR_MESSAGE,true);
		
		try {
			JOptionPane.showMessageDialog(null,new QEPUAssembler(args[0].replace(filesource_format,filebinary_format)).assemble(readFile(args[0])), "Assembled successfully", JOptionPane.INFORMATION_MESSAGE);
		} catch (HeadlessException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}*/
	}
}
