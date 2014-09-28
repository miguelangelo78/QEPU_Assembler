import java.io.BufferedReader;
import java.io.FileReader;
import javax.swing.JOptionPane;

public class qassembler {
	
	private static final String FILESOURCE_FORMAT=".qasm",FILEBINARY_FORMAT=".bin";
	
	public static void messagebox(String title,String msg,int message_type,boolean close){
		JOptionPane.showMessageDialog(null,msg,title,message_type);
		if(close) System.exit(1);
	}
	
	public static String readFile(String filepath){
		String text="";
		try {
			BufferedReader br=new BufferedReader(new FileReader(filepath));
			String line="";
			while ((line= br.readLine()) != null) text+=line+"\n";
			br.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return text;
	}
	
	public static void main(String[] args) {
		//String n="C:\\Users\\Miguel\\Desktop\\QEPU_Libs\\main.qasm";
		//JOptionPane.showMessageDialog(null,new QEPUAssembler(n.replace(FILESOURCE_FORMAT,FILEBINARY_FORMAT)).assemble(readFile(n)), "Assembled successfully", JOptionPane.INFORMATION_MESSAGE);
		
		if(args.length!=1) messagebox("Quantum Assembler Error","Usage: java -jar qassembler.jar \"C:\\....\\filename"+FILESOURCE_FORMAT+"\"",JOptionPane.ERROR_MESSAGE,true);
		if(!args[0].contains(FILESOURCE_FORMAT)) messagebox("Quantum Assembler Error","The file format is wrong. Use '"+FILESOURCE_FORMAT+"'",JOptionPane.ERROR_MESSAGE,true);
		new QEPUAssembler(args[0].replace(FILESOURCE_FORMAT,FILEBINARY_FORMAT)).assemble(readFile(args[0]));
	}
}
