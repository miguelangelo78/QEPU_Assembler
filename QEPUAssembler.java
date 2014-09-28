import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class QEPUAssembler {
	// GLOBAL VARIABLES:
    // TYPES OF OPERANDS:
    private static final int FUNC=0,OP1=1,OP2=2,OP3=3,REGISTER=4,QUBIT=5,MEMORY=6,FLAG=7,CONSTANT=8;
    //CONSTANTS:
    private static final int MAX_OPERAND_COUNT=3,BINARY_FILE_EOF=0xFF;
    private static final char STRING_TERMINATOR='$';
    private static final String FILESOURCE_FORMAT="qasm",FILEBINARY_FORMAT="bin",FILEMAIN_ENTRYPOINT="main";
    private static final String INVALID_INSTRUCTION_MSG="The instruction is malformed";
    //CONSTANT REGISTERS:
    //CARRIERS:
    private static final List<String[]> reg_carriers=Arrays.asList(new String[]{"ROC","R0"},new String[]{"RIC","R1"});   								  
    
    private int code_currline;
    private Map<Integer,Integer> code_lineoffsets;
    private Map<String,Integer> code_labels;
    private Map<String,int[]> code_variables;
    private int code_variables_address_start;
    private int file_linecount;
    private ArrayList<Object[]> include_files;
    private ArrayList<Integer> machinecode;
    
    private FileOutputStream mc_fos;
    private String mc_fullpath;
    
    private enum Instset{
        NULL, // BECAUSE THE ENUMS START AT 1 AND NOT 0
        MOQ,MOR,MOM,STR,LOD,CRW,CQW, // DATA MOVEMENT
        POP,PSH, // MEMORY STACK MOVEMENT
        CMT,CMP, // CONSTANT QUBIT DATA MOVEMENT
        CME, // COMPARE
        SEA,GEA, // SET AND GET FLAGS
        BES,BLW,BLE,BEQ,BGE,BGR,BDI,BZE,BNZ, // BRANCHING WITH OR WITHOUT JUMP STACK
        CALL,RET,JMP, // UNCONDICIONAL JUMPS WITH AND WITHOUT JUMP STACK
        ADD,ADDRK,SUB,SUBRK,SUBKR,MUL,MULRK,DIV,DIVRK,DIVKR,AND,ANDRK,ANDKR,OR,ORRK,ORKR,NOR,NORRK,NORKR,XOR,XORRK,XORKR,NAN,NANRK,NANKR,NOT,SHL,SHLRK,SHLKR,SHR,SHRRK,SHRKR, // ARITHMETIC AND LOGIC OPERATIONS
        INT, // SYSTEM CALLS
        DLY, // DELAY
        NOP, // DOES NOTHING (GOOD FOR DELAYS)
        HLT, // FINISHES THE PROGRAM
        X,Y,Z,H,S,T,ROX,ROY,ROZ, // 1 QUBIT GATE QUANTUM FUNCTION
        CNO,CSI,SWA,INC,DEC,SWQ,SWI, // 2 QUBIT GATE QUANTUM FUNCTION
        CSW,TOF,DEU // 3 QUBIT GATE QUANTUM FUNCTION
    }
    
    @SuppressWarnings("serial")
	private HashMap<String,int[]> dict=new HashMap<String,int[]>(){{
        //INSTRUCTION SET AND OPERAND TYPES:
        put("",new int[]{FUNC,0}); // THIS IS AN EMPTY LINE AND NEEDS TO BE IGNORED
        put("MOV",new int[]{FUNC,2});
        put("MOQ",new int[]{FUNC,2});
        put("MOR",new int[]{FUNC,2});
        put("MOM",new int[]{FUNC,2});
        put("STR",new int[]{FUNC,2});
        put("LOD",new int[]{FUNC,2});
        put("CRW",new int[]{FUNC,2});
        put("CQW*",new int[]{FUNC,2});
        put("POP",new int[]{FUNC,1});
        put("PSH",new int[]{FUNC,1});
        put("CMT",new int[]{FUNC,2});
        put("CMP",new int[]{FUNC,2});
        put("CME",new int[]{FUNC,2});
        put("SEA",new int[]{FUNC,1});
        put("GEA",new int[]{FUNC,1});
        put("BES",new int[]{FUNC,1});
        put("BLW",new int[]{FUNC,1});
        put("BLE",new int[]{FUNC,1});
        put("BEQ",new int[]{FUNC,1});
        put("BGE",new int[]{FUNC,1});
        put("BGR",new int[]{FUNC,1});
        put("BDI",new int[]{FUNC,1});
        put("BZE",new int[]{FUNC,1});
        put("BNZ",new int[]{FUNC,1});
        put("CALL",new int[]{FUNC,1});
        put("RET",new int[]{FUNC,0});
        put("JMP",new int[]{FUNC,1});
        put("ADD",new int[]{FUNC,3});
        put("SUB",new int[]{FUNC,3});
        put("MUL",new int[]{FUNC,3});
        put("DIV",new int[]{FUNC,3});
        put("AND",new int[]{FUNC,3});
        put("OR",new int[]{FUNC,3});
        put("NOR",new int[]{FUNC,3});
        put("XOR",new int[]{FUNC,3});
        put("NAN",new int[]{FUNC,3});
        put("NOT",new int[]{FUNC,2});
        put("SHL",new int[]{FUNC,3});
        put("SHR",new int[]{FUNC,3});
        put("INT",new int[]{FUNC,1});
        put("DLY",new int[]{FUNC,1});
        put("NOP",new int[]{FUNC,0});
        put("HLT",new int[]{FUNC,0});
        put("X",new int[]{FUNC,1});
        put("Y",new int[]{FUNC,1});
        put("Z",new int[]{FUNC,1});
        put("H",new int[]{FUNC,1});
        put("S",new int[]{FUNC,1});
        put("T",new int[]{FUNC,1});
        put("ROX",new int[]{FUNC,1});
        put("ROY",new int[]{FUNC,1});
        put("ROZ",new int[]{FUNC,1});
        put("CNO",new int[]{FUNC,2});
        put("CSI",new int[]{FUNC,2});
        put("SWA",new int[]{FUNC,2});
        put("INC",new int[]{FUNC,2});
        put("DEC",new int[]{FUNC,2});
        put("SWQ",new int[]{FUNC,2});
        put("SWI",new int[]{FUNC,2});
        put("CSW",new int[]{FUNC,3});
        put("TOF",new int[]{FUNC,3});
        put("DEU",new int[]{FUNC,3});
        put("R",new int[]{REGISTER,0});
        put("Q",new int[]{QUBIT,0});
        put("M",new int[]{MEMORY,0});
        put("F",new int[]{FLAG,0});
        put("@",new int[]{FUNC,1}); // LABELS
        put("$",new int[]{FUNC,3}); // VARIABLES ($varname content size)
        put("K",new int[]{CONSTANT,0}); // MAYBE WILL USE THIS*/
    }};    
   
    //FUNCTIONS:
    public QEPUAssembler(String mc_fullpath){
    	this.mc_fullpath=mc_fullpath;
    	file_linecount=0;
    	include_files=new ArrayList<Object[]>();
    	machinecode=new ArrayList<Integer>();
    	
    	code_currline=0;
        code_lineoffsets=new HashMap<Integer,Integer>();
        code_labels=new HashMap<String,Integer>();
        code_variables=new HashMap<String,int[]>();
        code_variables_address_start=0;
    }
    
    public void set_file_linecount(String assembly){
    	Matcher line_count_match=Pattern.compile("$",Pattern.MULTILINE).matcher(assembly);
    	while(line_count_match.find()) file_linecount++; file_linecount--;
    }
    
    public void create_file(){
    	try {
            mc_fos=new FileOutputStream(mc_fullpath);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(QEPUAssembler.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public void close_file(){
        try {
            mc_fos.close();
        } catch (IOException ex) {
            Logger.getLogger(QEPUAssembler.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public String getFilename(String filepath){
    	Matcher fileNameMatcher=Pattern.compile("(?:.+\\\\)?(.+?)\\.(?:"+FILESOURCE_FORMAT+"|"+FILEBINARY_FORMAT+")").matcher(filepath);
    	if(fileNameMatcher.find()) return fileNameMatcher.group(1);
    	return "NULL";
    }
    
    public void create_linkedfile(String linked_assembly){
    	try {
			BufferedWriter la_writer=new BufferedWriter(new FileWriter(new File(mc_fullpath.replace(getFilename(mc_fullpath)+"."+FILEBINARY_FORMAT, getFilename(mc_fullpath)+"_linked."+FILESOURCE_FORMAT))));
			la_writer.write(linked_assembly);
			la_writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
    }
    
    public void create_binaryfile(){
    	create_file();
    	insert_machinecode(BINARY_FILE_EOF, BINARY_FILE_EOF, BINARY_FILE_EOF, BINARY_FILE_EOF); // EOF
        try{
	    	for(int i=0;i<machinecode.size();i++)
	    		if(i%4==0) mc_fos.write(machinecode.get(i));
	    		else mc_fos.write((ByteBuffer.allocate(4).putInt(machinecode.get(i)).array()));	
    	}catch(Exception e){}
       	close_file();
    }
    
    public void insert_machinecode(int func,int op1,int op2,int op3){
    	machinecode.addAll(Arrays.asList(func,op1,op2,op3));
    }
    
    public String fix_str_newlines(String src){
        StringBuilder strBldr=new StringBuilder(src);
        while(src.contains("\\n")){
            int newline_index=src.indexOf("\\n");
            strBldr.setCharAt(newline_index, (char)10);
            strBldr.deleteCharAt(newline_index+1);
            src=strBldr.toString();
        }
        return src;
    }

    public String extractType(String operand) throws Exception{
        String type="NULLTYPE";
        try{
        	Matcher type_matcher=Pattern.compile("((?:\\*)?(?:[ |	]+?)?[m|r|q])[0-9]+|(?:([0-9]+))|((?:\\*)?(?:[ |	]+?)?[@|$|\"|']+).+?$",Pattern.CASE_INSENSITIVE).matcher(operand); type_matcher.find();
        	if(type_matcher.group(1)!=null) type=type_matcher.group(1).toUpperCase(); //USING M|R|Q
        	else if(type_matcher.group(2)!=null) type="K"; //USING NUMBERS
        	else if(type_matcher.group(3)!=null){ //USING @|$|"|'
        		String operand_specialchar=type_matcher.group(3);
        		if(operand_specialchar.contains("@")) type="L";
        		if(operand_specialchar.contains("$")) type="V";
        		if(operand_specialchar.contains("\"")) type="S";
        		if(operand_specialchar.contains("'")) type="C";
        		if(operand_specialchar.contains("*")) type="*"+type;
        	}
        }catch(Exception e){}
        return type;
    }
    
    public int extractNumber(String operand){
        Matcher matcher=Pattern.compile("[0-9]+").matcher(operand); matcher.find();
        return Integer.parseInt(matcher.group(0));
    }
    
    public int getJumpOffset(int jump_address){
        int new_jump_address=jump_address;
        for(Integer key:code_lineoffsets.keySet()) if(jump_address>key) new_jump_address+=code_lineoffsets.get(key)-1;
        new_jump_address--;
        return new_jump_address;
    }
    
    public void setLineOffsets(String assembly){
    	// SET OFFSETS FOR: STRINGS, VARIABLES AND INTERVALS
    	String[] assembly_splitted=assembly.split("\n");
    	for(int i=0;i<assembly_splitted.length;i++){
	    	Matcher line_match=Pattern.compile("(?:\\$.+? ['|\"]?(.+?)['|\"]? ([0-9]+))|\"(.+?)\"|([0-9]+)(?:[ |	]+?)?-+(?:[ |	]+?)?([0-9]+)").matcher(assembly_splitted[i]);
    		while(line_match.find())
	    		if(line_match.group(1)!=null)
	    			code_lineoffsets.put(i+1, Integer.parseInt(line_match.group(2))); // SET OFFSET FOR VARIABLES
				else if(line_match.group(3)!=null)
					code_lineoffsets.put(i+1, fix_str_newlines(line_match.group(3)).length()+1); // SET OFFSET FOR STRINGS
				else if(line_match.group(4)!=null)
					code_lineoffsets.put(i+1,Math.abs(Integer.parseInt(line_match.group(4))-Integer.parseInt(line_match.group(5))+1)); // SET OFFSET FOR INTERVALS
		}
    }
    
    public boolean include_file_isincluded(String include_filename){
    	for(int i=0;i<include_files.size();i++) if(include_files.get(i)[0].equals(include_filename)) return true;
    	return false;
    }
    
    public String handle_intervals(String assembly) throws Exception{
    	Pattern intervals_patt=Pattern.compile("([0-9]+)(?:[ |	]+?)?-+(?:[ |	]+?)?([0-9]+)",Pattern.CASE_INSENSITIVE|Pattern.MULTILINE);
    	for(String line:assembly.split("\\n")){
    		Matcher intervals_match=intervals_patt.matcher(line.trim());
    		String interval_substitute="";
    		
    		while(intervals_match.find()){
    			int start_interval=Integer.parseInt(intervals_match.group(1).trim());
        		int end_interval=Integer.parseInt(intervals_match.group(2).trim());
    			
        		//IF THERE'S 1 INTERVAL:
        		if(start_interval<=end_interval)
            		for(int i=start_interval;i<=end_interval;i++) interval_substitute+=line.replaceFirst(intervals_match.group(0),Integer.toString(i)+" ")+"\n";
        		else
        			for(int i=start_interval;i>=end_interval;i--) interval_substitute+=line.replaceFirst(intervals_match.group(0),Integer.toString(i)+" ")+"\n";
        		
        		//IF THERE'S MORE THAN 1 INTERVAL:
        		while(intervals_match.find()){
        			start_interval=Integer.parseInt(intervals_match.group(1).trim());
        			end_interval=Integer.parseInt(intervals_match.group(2).trim());
        			
        			if(start_interval<=end_interval)
            			for(int i=start_interval;i<=end_interval;i++) interval_substitute=interval_substitute.replaceFirst(intervals_match.group(0), Integer.toString(i)+" ");
            		else 
            			for(int i=start_interval;i>=end_interval;i--) interval_substitute=interval_substitute.replaceFirst(intervals_match.group(0), Integer.toString(i)+" ");
            	}
        		
        		//CHECK FOR LEFTOVER INTERVALS:
        		intervals_match=intervals_patt.matcher(interval_substitute);
        		while(intervals_match.find()) interval_substitute=interval_substitute.replaceAll(".+?"+intervals_match.group(0)+"\n", "");
        		
        		//LINE IS PREPARED TO BE REPLACED BY THE PROPER INTERVAL:
        		assembly=assembly.replace(line+"\n",interval_substitute);
        		break;
        	}
    	}
    	return assembly;
    }
    
    public void include_file(String include_name){
    	try {
    		System.out.println("Linking '"+include_name+"'...");
    		int include_code_linecount=0;
        	String include_code="";
    		String line="";
    		BufferedReader br=new BufferedReader(new FileReader(mc_fullpath.replace(getFilename(mc_fullpath)+"."+FILEBINARY_FORMAT,include_name)));
    		while ((line= br.readLine()) != null){include_code+=line+"\n";include_code_linecount++;}
    		br.close();
			include_files.add(new Object[]{include_name,include_code,include_code_linecount});
		} catch (Exception e) {
			e.printStackTrace();
		}
    }
    
    public String handle_including(String assembly)throws Exception{
    	//HANDLE INCLUDE FILES - BEGIN
    	boolean including_done=false;
    	while(!including_done){
    		Matcher include_match=Pattern.compile("^(?:.+?)?get.+?([a-z|A-Z].+?"+FILESOURCE_FORMAT+")",Pattern.MULTILINE).matcher(assembly);
    		int files_included=0;
        	while(include_match.find()){
        		String include_filename=include_match.group(1);
        		if(include_file_isincluded(include_filename) && include_filename.equals(getFilename(mc_fullpath)+"."+FILESOURCE_FORMAT)){
        			String error_message="Cannot include the file itself ("+include_filename+")! Exiting...";
        			System.err.println(error_message);
        			throw new Exception(error_message);
        		}
        		else
        			if(!include_file_isincluded(include_filename)) include_file(include_filename);
	        		else System.err.println("The file '"+include_filename+"' was previously included. Ignoring this line.");
        		assembly=assembly.replace(include_match.group(0),"");
        		files_included++;
        	}
        	if(files_included==0){including_done=true;break;}
        	for(int i=include_files.size()-1;i>=0;i--)
        		assembly="#SOURCE BEGIN:"+include_files.get(i)[0]+"\n"+(String)(include_files.get(i)[1])+"#SOURCE END: "+include_files.get(i)[0]+"\n"+assembly; // INSERT INCLUDE FILES IN THE BEGINNING OF THE MAIN FILE
        }
    	//HANDLE INCLUDE FILES - END
    	if(include_files.size()>0){
    		//SEE IF MAIN LABEL HAS BEEN DECLARED - BEGIN
	    	Matcher mainLabel_matcher=Pattern.compile("^@(?:.+?)?"+FILEMAIN_ENTRYPOINT,Pattern.MULTILINE).matcher(assembly);
	    	int mainLabels_declared=0; while(mainLabel_matcher.find()) mainLabels_declared++;
	    	if(mainLabels_declared==0) throw new Exception("The label '@"+FILEMAIN_ENTRYPOINT+"' has not been declared in the main file!");
	    	if(mainLabels_declared>1) throw new Exception("The label '@"+FILEMAIN_ENTRYPOINT+"' cannot be declared multiple times!");
	    	assembly="jmp @main\n"+assembly;
	    	create_linkedfile(assembly); // CREATE FILE RESULT AFTER THE LINKING
            //END
    	}
    	return assembly;
    }
   
    public void declare_labels(String assembly){
    	int line_ctr=0;
    	for(String line:assembly.split("\\n")){
    		line=line.trim();
    		if(line.length()>0 && line.charAt(0)=='@') code_labels.put(line.replace("@",""),line_ctr+1); // DECLARE LABELS THAT ARE DECLARED IN THE WHOLE FILE
    		line_ctr++;
    	}
    }
    
    public String getErrorMessage(int original_errorline){
    	String error_file="";
        int error_line=original_errorline;
        int included_linesaccumulated=0;
        if(include_files.size()>0) included_linesaccumulated++;
        for(int i=0;i<include_files.size();i++){
        	included_linesaccumulated+=(Integer)include_files.get(i)[2]+2;
        	if(code_currline<included_linesaccumulated){ 
        		included_linesaccumulated++;
        		error_file=(String)include_files.get(i)[0];
        		error_line-=2;
        		break;
        	}
        	error_line-=(Integer)include_files.get(i)[2]+2;
        }
        String error_message="There was an error in ";
        if(include_files.size()>0 && code_currline<included_linesaccumulated)
        	error_message+="the file: '"+error_file+"' in "+((error_line<0)?"the assemble time":"the line: "+error_line);
        else error_message+=" the line: "+(code_currline-included_linesaccumulated);
        error_message+=". ";
        return error_message;
    }
    
    public String handle_comments(String assembly){
    	Matcher comment_matches=Pattern.compile("(?:(\\#.+?)$)|(\\/\\*(?:.|\n)+?\\*\\/)",Pattern.MULTILINE).matcher(assembly);
    	while(comment_matches.find()){
    		if(comment_matches.group(1)!=null) //USING 1 LINE COMMENTS
    			assembly=assembly.replaceAll(comment_matches.group(1),"");
    		else{ //USING MULTILINE COMMENTS
    			String multiline_replacement="";
    			for(int i=0;i<comment_matches.group(2).split("\n").length;i++) multiline_replacement+="\n";
    			assembly=assembly.replace("\n"+comment_matches.group(2),multiline_replacement);
    		}
    	}
    	return assembly;
    }
    
    public String assemble(String assembly){
    	System.out.println("Assembling "+mc_fullpath+"...");
        String success="ERROR: UNASSEMBLED";
        try{	
        	set_file_linecount(assembly);
        	assembly=handle_including(assembly.trim());
        	assembly=handle_comments(assembly);
        	assembly=handle_intervals(assembly);
        	setLineOffsets(assembly);
        	declare_labels(assembly);
        	
        	//Read all lines and iterate through them:
            ArrayList<String> codelines=new ArrayList<>();
            for(String line:assembly.split("\\n")) codelines.add(line.trim());
            
            //Translate all lines to machine code:
            Pattern line_patt=Pattern.compile("^(?:\\$|@|#)|(?:\".+?\")|[a-z|A-Z|\\d|*|_|'|\\-|$|@|#]+?(?:	|\\."+FILESOURCE_FORMAT+"| |'|\"|\n|$)"); // OPERAND PATTERN
            for(code_currline=0;code_currline<codelines.size();code_currline++){
                //FETCH FUNC,OP1,OP2 AND OP3:
                String currline=codelines.get(code_currline);
                Matcher matcher=line_patt.matcher(currline);
                ArrayList<String> operands_arrlist=new ArrayList<>();
                while(matcher.find()) operands_arrlist.add(matcher.group(0).trim());
                String [] operands=operands_arrlist.toArray(new String[operands_arrlist.size()]);
                
                if(currline.length()<=0 || currline.charAt(0)=='#'){
                    insert_machinecode(Instset.NOP.ordinal(), 0, 0, 0);
                    continue;
                }
                
                //VALIDATE FUNCTION:
                String function=operands[FUNC].toUpperCase();
                if(!dict.containsKey(function) || dict.get(function)[0]!=FUNC) // DOES THIS FUNCTION EXIST IN THE DICTIONARY?
                    throw new Exception("The instruction '"+operands[FUNC]+"' is unrecognizable");
                if(dict.get(function)[1]!=operands.length-1)
                    throw new Exception("The instruction '"+operands[FUNC]+"' has incorrect size. You gave "+(operands.length-1)+" operand"+((operands.length-1>1)?"s":"")+". It needs "+dict.get(function)[1]+" operands");
                
                //INSTRUCTION MAY BE VALID AT THIS POINT, FETCH OPERAND TYPES:
                String [] op_types=new String[operands.length-1];
                for(int i=0;i<op_types.length;i++){
                    operands[i+1]=operands[i+1].trim();
                    for(int j=0;j<reg_carriers.size();j++) //REPLACE ALL CONSTANT OPERANDS THAT MAY BE USED (ROC AND RIC)
                        if(operands[i+1].toUpperCase().equals(reg_carriers.get(j)[0])){
	                    	operands[i+1]=reg_carriers.get(j)[1]; break;
                    	}
                    op_types[i]=extractType(operands[i+1]);
                    operands[i+1]=operands[i+1].replaceAll("'|\"", "");
                }
                
                //TRANSFORM OPERAND POINTERS AND CHARACTERS INTO CONSTANT NUMBERS (WHICH IS THE ADDRESS OF THE POINTER):
                for(int i=0;i<MAX_OPERAND_COUNT;i++)
	                try{
	                	if(op_types[i].equals("*M") || op_types[i].equals("*R") || op_types[i].equals("*Q") || op_types[i].equals("*L") ||  op_types[i].equals("*V") || op_types[i].equals("C")){
		            		if(op_types[i].equals("*L")) operands[i+1]=""+code_labels.get(operands[i+1].replaceAll("\\*|@",""));
		            		else if(op_types[i].equals("*V")) operands[i+1]=""+code_variables.get(operands[i+1].replaceAll("\\*",""))[0];
		            		else if(op_types[i].equals("C")) operands[i+1]=Integer.toString((int)operands[i+1].charAt(0));
		            		else operands[i+1]=""+extractNumber(operands[i+1]);
		            		op_types[i]="K";
		            	}
	                }catch(Exception e){}
            	
                //TRANSLATE FUNCTIONS:
                switch(function){ // FUNCTION OPERAND
                    case "$": // VARIABLE DECLARATION
                        if(!op_types[2].equals("K")) throw new Exception("The size operand (3rd op.) must be a number");
                        int var_bytelength=extractNumber(operands[OP3]);
                        if(var_bytelength<=0) throw new Exception("The size operand (3rd op.) must be greater than 0");
                        
                        if(op_types[1].equals("V")){ // $ VAR1 = $VAR2
                            String var1=operands[OP1];
                            String var2=operands[OP2];
                            int var2_bytelength=code_variables.get(var2)[1];
                            
                            if(var2_bytelength>var_bytelength) throw new Exception("The variable '"+var2+"' is bigger than the variable '"+var1+"'");
                            code_variables.put(var1,new int[]{code_variables_address_start,var_bytelength});
                        
                            for(int i=0;i<var_bytelength;i++)
                                if(i<var2_bytelength) insert_machinecode(Instset.MOM.ordinal(), code_variables.get(var1)[0]+i, code_variables.get(var2)[0]+i, 0);
                                else insert_machinecode(Instset.CRW.ordinal(), code_variables.get(var1)[0]+i,0, 0);
                        }else
                        if(op_types[1].equals("S")){ // $VAR=STRING
                            operands[OP2]=fix_str_newlines(operands[OP2])+STRING_TERMINATOR;
                            if(var_bytelength<operands[OP2].length()) throw new Exception("The variable's size is too small for the string");
                            for(int i=0;i<operands[OP2].length();i++) // WRITE THE CONTENT
                                insert_machinecode(Instset.CRW.ordinal(),code_variables_address_start+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                            for(int i=operands[OP2].length();i<var_bytelength;i++) // WRITE THE REST (EMPTY SPACE RESERVED FOR THE VARIABLE)
                                insert_machinecode(Instset.CRW.ordinal(),code_variables_address_start+i, 0, 0);
                        }
                        else if(op_types[1].equals("K")){ // $VAR=NUMBER
                            insert_machinecode(Instset.CRW.ordinal(),code_variables_address_start,extractNumber(operands[OP2]),0); // WRITE THE CONTENT
                            for(int i=1;i<var_bytelength;i++) // WRITE THE REST (EMPTY SPACE RESERVED FOR THE VARIABLE)
                                insert_machinecode(Instset.CRW.ordinal(), code_variables_address_start+i, 0, 0);
                        }
                        else throw new Exception("The operands are wrong");
                        
                        operands[OP1]="$"+operands[OP1].trim();
                        if(!code_variables.containsKey(operands[OP1])) code_variables.put(operands[OP1],new int[]{code_variables_address_start,var_bytelength});
                        code_variables_address_start+=var_bytelength;
                        break;
                    case "@": // LABEL DECLARATION
                        if(!code_labels.containsKey(operands[OP1])) code_labels.put(operands[OP1],code_currline+1);
                        insert_machinecode(Instset.NOP.ordinal(), 0, 0, 0);
                        break;
                    case "MOV":
                        // DECIDE WHETHER IT IS A LOD, STR, MOR, MOM, CMT, CMP, CRW OR CQW
                    	if(op_types[0].equals("V") && op_types[1].equals("S")){ // V S ->
                            if(!code_variables.containsKey(operands[OP1])) throw new Exception("The variable '"+operands[OP1]+"' was not declared");
                            operands[OP2]=fix_str_newlines(operands[OP2])+STRING_TERMINATOR;
                            int var_bytecount=code_variables.get(operands[OP1])[1];
                            if(var_bytecount<operands[OP2].length()) throw new Exception("The string is too large for the variable '"+operands[OP1]+"'");
                            for(int i=0;i<var_bytecount;i++)
                                if(i<operands[OP2].length()) insert_machinecode(Instset.CRW.ordinal(), code_variables.get(operands[OP1])[0]+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                                else insert_machinecode(Instset.CRW.ordinal(), code_variables.get(operands[OP1])[0]+i, 0, 0);
                        }
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("K")) // V K
                            insert_machinecode(Instset.CRW.ordinal(),code_variables.get(operands[OP1])[0],extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("M")) // V M
                            insert_machinecode(Instset.MOM.ordinal(),code_variables.get(operands[OP1])[0],extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("V")){ // V V -> INCORRECT (RESERVE MORE SPACE FOR THE VARIABLES)
                            int var1_bytecount=code_variables.get(operands[OP1])[1];
                            int var2_bytecount=code_variables.get(operands[OP2])[1];
                            if(var1_bytecount<var2_bytecount) throw new Exception("The variable '"+operands[OP2]+"' is bigger than the variable '"+operands[OP1]+"'");
                            for(int i=0;i<var1_bytecount;i++)
                                if(i<var2_bytecount) insert_machinecode(Instset.MOM.ordinal(),code_variables.get(operands[OP1])[0]+i,code_variables.get(operands[OP2])[0]+i,0);
                                else insert_machinecode(Instset.CRW.ordinal(),code_variables.get(operands[OP1])[0]+i,0,0);
                        }
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("V")) // M V
                            insert_machinecode(Instset.MOM.ordinal(),extractNumber(operands[OP1]),code_variables.get(operands[OP2])[0],0);
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("V")) // R V
                            insert_machinecode(Instset.LOD.ordinal(),extractNumber(operands[OP1]),code_variables.get(operands[OP2])[0],0);
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("R")) // V R
                            insert_machinecode(Instset.STR.ordinal(),code_variables.get(operands[OP1])[0],extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("M")) // M M
                            insert_machinecode(Instset.MOM.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("R")) // R R
                            insert_machinecode(Instset.MOR.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("M")) // R M
                            insert_machinecode(Instset.LOD.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("R")) // M R
                            insert_machinecode(Instset.STR.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("QT") && op_types[1].equals("K")) // QT K
                            insert_machinecode(Instset.CMT.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("QP") && op_types[1].equals("K")) // QP K
                            insert_machinecode(Instset.CMP.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("K")) // R K
                            insert_machinecode(Instset.CQW.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("K")) // M K
                            insert_machinecode(Instset.CRW.ordinal(),extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("S")){ // M S
                            operands[OP2]=fix_str_newlines(operands[OP2])+STRING_TERMINATOR;
                            for(int i=0;i<operands[OP2].length();i++)
                                insert_machinecode(Instset.CRW.ordinal(),extractNumber(operands[OP1])+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                        }
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("S")){ // R S
                            operands[OP2]=fix_str_newlines(operands[OP2])+STRING_TERMINATOR;
                            for(int i=0;i<operands[OP2].length();i++)
                                insert_machinecode(Instset.CQW.ordinal(),extractNumber(operands[OP1])+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                        }
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;
                    case "POP":
                        if(op_types[0].equals("R")) insert_machinecode(Instset.POP.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;
                    case "PSH":
                        if(op_types[0].equals("R")) insert_machinecode(Instset.PSH.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;
                    case "CMT":
                        if(op_types[0].equals("QT") && op_types[1].equals("K"))
                            insert_machinecode(Instset.CMT.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;
                    case "CMP":
                        if(op_types[0].equals("QT") && op_types[1].equals("K"))
                            insert_machinecode(Instset.CMP.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;
                    case "CME": 
                        if(op_types[0].equals("R") && op_types[1].equals("R"))
                            insert_machinecode(Instset.CME.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;    
                    case "SEA": 
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.SEA.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;    
                    case "GEA": 
                    	if(op_types[0].equals("K"))
                            insert_machinecode(Instset.GEA.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;    
                    case "BES": 
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BES.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BLW": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BLW.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BLW.ordinal(),getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;    
                    case "BLE": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BLE.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BLW.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BEQ": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BEQ.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BEQ.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BGE": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BGE.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BGE.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BGR": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BGR.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BGR.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BDI": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BDI.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BDI.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BZE": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BZE.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BZE.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "BNZ": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BNZ.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BNZ.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "CALL": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.CALL.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.CALL.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "RET": 
                        insert_machinecode(Instset.RET.ordinal(), 0, 0, 0);
                        break;   
                    case "JMP": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.JMP.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.JMP.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;
                    case "ADD":
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.ADD.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if((op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")))
                                insert_machinecode(Instset.ADDRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.ADDRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP3]), extractNumber(operands[OP2]));
                                else
                                    throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "SUB": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R")) //M M M
                            insert_machinecode(Instset.SUB.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.SUBRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.SUBKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "MUL": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.MUL.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if((op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K"))) //  M M K
                                insert_machinecode(Instset.MULRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.MULRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP3]), extractNumber(operands[OP2]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "DIV": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.DIV.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.DIVRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.DIVKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "AND": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.AND.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.ANDRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.ANDKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "OR": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.OR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.ORRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.ORKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "NOR": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.NOR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.NORRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.NORKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "XOR": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.XOR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.XORRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.XORKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "NAN": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.NAN.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.NANRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.NANKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "NOT": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.NOT.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "SHL": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.SHL.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.SHLRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.SHLKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "SHR": 
                        if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("R"))
                            insert_machinecode(Instset.SHR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("R") && op_types[1].equals("R") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.SHRRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("R") && op_types[1].equals("K") && op_types[2].equals("R")) // M K M
                                    insert_machinecode(Instset.SHRKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "INT":
                        if(op_types[0].equals("K")) insert_machinecode(Instset.INT.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "DLY": 
                        if(op_types[0].equals("K")) insert_machinecode(Instset.DLY.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "NOP": 
                        insert_machinecode(Instset.NOP.ordinal(), 0, 0, 0);
                        break;   
                    case "HLT": 
                        insert_machinecode(Instset.HLT.ordinal(), 0, 0, 0);
                        break;   
                    case "X": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.X.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "Y": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.Y.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "Z": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.Z.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "H": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.H.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "S": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.S.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "T": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.T.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "ROX": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.ROX.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "ROY": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.ROY.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "ROZ": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.ROZ.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "CNO": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.CNO.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "CSI": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.CSI.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "SWA": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.SWA.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "INC": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.INC.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "DEC": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.DEC.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "SWQ": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.SWQ.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "SWI": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.SWI.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "CSW": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q") && op_types[2].equals("Q")) 
                            insert_machinecode(Instset.CSW.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),extractNumber(operands[OP3]));
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "TOF": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q") && op_types[2].equals("Q")) 
                            insert_machinecode(Instset.TOF.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),extractNumber(operands[OP3]));
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "DEU": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q") && op_types[2].equals("Q")) 
                            insert_machinecode(Instset.DEU.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),extractNumber(operands[OP3]));
                        else throw new Exception(INVALID_INSTRUCTION_MSG);
                        break;   
                    case "": break; // IGNORE EMPTY LINE
                    default: break;
                }
            }
        }catch(Exception e){
            code_currline++;
            success=getErrorMessage(code_currline)+e.getMessage();
            System.err.println(success);
            e.printStackTrace();
            return success;
        }
        insert_machinecode(Instset.HLT.ordinal(), 0, 0, 0);
        create_binaryfile();
        success="Your code has been successfully assembled ("+file_linecount+" lines)";
        System.out.println("Done!");
        return success;
    }
}