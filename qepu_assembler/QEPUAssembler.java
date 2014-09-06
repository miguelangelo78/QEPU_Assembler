package qepu_assembler;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class QEPUAssembler {
    // TYPES OF OPERANDS:
    private static final int FUNC=0,OP1=1,OP2=2,OP3=3,
                             REGISTER=4,QUBIT=5,MEMORY=6,FLAG=7,CONSTANT=8,VARIABLE=9;
    private static final int INSTR_WIDTH=13,MAX_OPERAND_COUNT=3;
    private static final char STRING_TERMINATOR='$',LABEL_TYPE='@',VAR_TYPE='$';
    
    private int code_currline;
    private Map<Integer,Integer> code_lineoffsets;
    private Map<String,Integer> code_labels;
    private Map<String,int[]> code_variables;
    private int code_variables_address_start;
    
    private FileOutputStream mc_fos;
    private final String mc_filename;
    private final String mc_filepath;
    private final String mc_fileformat;
    private final String mc_fullpath;
    
    private enum Instset{
        NULL, // BECAUSE THE ENUMS START AT 1 AND NOT 0
        MOQ,MOR,MOM,STR,LOD,CRW,CQW, // DATA MOVEMENT
        POP,PSH, // MEMORY STACK MOVEMENT
        CMT,CMP, // CONSTANT QUBIT DATA MOVEMENT
        CME, // COMPARE
        SEF,GEF, // SET AND GET FLAGS
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
        put("SEF",new int[]{FUNC,2});
        put("GEF",new int[]{FUNC,2});
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
        put("$",new int[]{FUNC,2}); // VARIABLES
        put("K",new int[]{CONSTANT,0}); //MAYBE WILL USE THIS*/
    }};
    
    public QEPUAssembler(){
        mc_filepath="C:\\Users\\Miguel\\Desktop\\";
        mc_filename="test";
        mc_fileformat="bin";
        mc_fullpath=mc_filepath+mc_filename+"."+mc_fileformat;
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
    
    public void insert_machinecode(int func,int op1,int op2,int op3){
        try{
            mc_fos.write(func);
            mc_fos.write(ByteBuffer.allocate(4).putInt(op1).array());
            mc_fos.write(ByteBuffer.allocate(4).putInt(op2).array());
            mc_fos.write(ByteBuffer.allocate(4).putInt(op3).array());
        }catch(Exception e){e.printStackTrace();}
    }
    
    public String extractType(String operand) throws Exception{
        Matcher matcher=Pattern.compile("[a-z|A-Z]+").matcher(operand); matcher.find();
        String match;
        if(operand.charAt(0)==LABEL_TYPE) match="L"; // THE OPERAND IS A LABEL
        else if(operand.charAt(0)==VAR_TYPE) match="V"; // THE OPERAND IS A VARIABLE
        else if(operand.contains("\"")) match="S"; // THE OPERAND IS A STRING
        else if(operand.contains("'")) match="C"; // THE OPERAND IS A CHARACTER
        else
            try{
                match=matcher.group(0).toUpperCase(); // THE OPERAND IS A REGULAR TYPE
            }catch(Exception e){
                match="K"; // THE OPERAND IS A NUMBER
            }
        return match;
    }
    
    public int extractNumber(String operand){
        Matcher matcher=Pattern.compile("[0-9]+").matcher(operand); matcher.find();
        return Integer.parseInt(matcher.group(0));
    }
    
    public int getJumpOffset(int jump_address){
        int new_jump_address=jump_address;
        for(Integer key:code_lineoffsets.keySet()) if(jump_address>key) new_jump_address+=code_lineoffsets.get(key)-1;
        return new_jump_address-1;
    }
    
    public int assemble(String assembly){
        create_file();
        
        code_currline=0;
        code_lineoffsets=new HashMap<Integer,Integer>();
        code_labels=new HashMap<String,Integer>();
        code_variables=new HashMap<String,int[]>();
        code_variables_address_start=0;
        
        int success=0;
        try{
            //TODO: read all lines and iterate through them
            ArrayList<String> codelines=new ArrayList<>();
            for(String line:assembly.split("\\n")){
                String line_sanitized=line.trim();
                if(line_sanitized.length()>0 && line_sanitized.charAt(0)!='#') codelines.add(line_sanitized);
            }
            
            Pattern line_patt=Pattern.compile("^(?:\\$|@|#)|(?:\".+?\")|[a-z|A-Z|\\d|_|'|$|@|#]+?(?: |'|\"|\n|$)"); // OPERAND PATTERN
            
            for(code_currline=0;code_currline<codelines.size();code_currline++){
                //FETCH FUNC,OP1,OP2 AND OP3:
                Matcher matcher=line_patt.matcher(codelines.get(code_currline).trim());
                ArrayList<String> operands_arrlist=new ArrayList<>();
                while(matcher.find()) operands_arrlist.add(matcher.group(0).trim());
                String [] operands=operands_arrlist.toArray(new String[operands_arrlist.size()]);
                
                String function=operands[FUNC].toUpperCase();
                if(!dict.containsKey(function) || dict.get(function)[0]!=FUNC || dict.get(function)[1]!=operands.length-1) // DOES THIS FUNCTION EXIST IN THE DICTIONARY?
                    throw new Exception("The instruction is unrecognizable");
                
                //INSTRUCTION MAY BE VALID (FETCH OPERAND TYPES)
                String [] op_types=new String[operands.length-1];
                for(int i=0;i<op_types.length;i++){
                    op_types[i]=extractType(operands[i+1]);
                    operands[i+1]=operands[i+1].replaceAll("'|\"|\\$", "");
                }
                
                switch(function){ // FUNCTION OPERAND
                    case "$": // VARIABLE DECLARATION
                        int var_bytelength=0;
                        if(op_types[1].equals("S")){
                            var_bytelength=operands[OP2].length()+1;
                            operands[OP2]+=STRING_TERMINATOR;
                            for(int i=0;i<operands[OP2].length();i++)
                                insert_machinecode(Instset.CRW.ordinal(),code_variables_address_start+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                            code_lineoffsets.put(code_currline+1,operands[OP2].length());
                        }else if(op_types[1].equals("C")){
                            var_bytelength=1;
                            if(operands[OP2].length()!=1) throw new Exception("The second operand has incorrect size");
                            else insert_machinecode(Instset.CRW.ordinal(), code_variables_address_start,extractNumber(""+((int)operands[OP2].charAt(0))), 0);
                        }else if(op_types[1].equals("K")){
                            var_bytelength=1;
                            insert_machinecode(Instset.CRW.ordinal(),code_variables_address_start,extractNumber(operands[OP2]),0);
                        }else throw new Exception("The operands are wrong");
                        
                        code_variables.put(operands[OP1],new int[]{code_variables_address_start,var_bytelength});
                        code_variables_address_start+=var_bytelength;
                        break;
                    case "@": // LABEL DECLARATION
                        code_labels.put(operands[OP1],code_currline+1);
                        insert_machinecode(Instset.NOP.ordinal(), 0, 0, 0);
                        break;
                    case "MOV":
                        // TODO: DECIDE WHETHER IT IS A LOD, STR, MOR, MOM, CMT, CMP, CRW OR CQW
                        if(op_types[0].equals("V") && op_types[1].equals("S")){ // V S -> INCORRECT (RESERVE MORE SPACE FOR THE VARIABLES)
                            operands[OP2]+=STRING_TERMINATOR;
                            for(int i=0;i<operands[OP2].length();i++)
                                insert_machinecode(Instset.CRW.ordinal(),code_variables.get(operands[OP1])[0]+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                            code_lineoffsets.put(code_currline+1,operands[OP2].length());
                        }
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("C")) // V C
                            if(operands[OP2].length()!=1) throw new Exception("The second operand has incorrect size");
                            else insert_machinecode(Instset.CRW.ordinal(),code_variables.get(operands[OP1])[0],extractNumber(""+((int)operands[OP2].charAt(0))), 0);
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("K")) // V K
                            insert_machinecode(Instset.CRW.ordinal(),code_variables.get(operands[OP1])[0],extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("M")) // V M
                            insert_machinecode(Instset.MOM.ordinal(),code_variables.get(operands[OP1])[0],extractNumber(operands[OP2]),0);
                        else
                        if(op_types[0].equals("V") && op_types[1].equals("V")) // V V -> INCORRECT (RESERVE MORE SPACE FOR THE VARIABLES)
                            insert_machinecode(Instset.MOM.ordinal(),code_variables.get(operands[OP1])[0],code_variables.get(operands[OP2])[0],0);
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("V")) // M V
                            insert_machinecode(Instset.MOM.ordinal(),extractNumber(operands[OP1]),code_variables.get(operands[OP2])[0],0);
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("V")) // R V
                            insert_machinecode(Instset.CQW.ordinal(),extractNumber(operands[OP1]),code_variables.get(operands[OP2])[0],0);
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
                        if(op_types[0].equals("M") && op_types[1].equals("C")) // M C
                            if(operands[OP2].length()!=1) throw new Exception("The second operand has incorrect size");
                            else insert_machinecode(Instset.CRW.ordinal(), extractNumber(operands[OP1]),extractNumber(""+((int)operands[OP2].charAt(0))), 0);
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("C")) // R C
                            if(operands[OP2].length()!=1) throw new Exception("The second operand has incorrect size");
                            else insert_machinecode(Instset.CQW.ordinal(), extractNumber(operands[OP1]),extractNumber(""+((int)operands[OP2].charAt(0))), 0);
                        else
                        if(op_types[0].equals("M") && op_types[1].equals("S")){ // M S
                            operands[OP2]+=STRING_TERMINATOR;
                            for(int i=0;i<operands[OP2].length();i++)
                                insert_machinecode(Instset.CRW.ordinal(),extractNumber(operands[OP1])+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                            code_lineoffsets.put(code_currline+1,operands[OP2].length());
                        }
                        else
                        if(op_types[0].equals("R") && op_types[1].equals("S")){ // R S
                            operands[OP2]+=STRING_TERMINATOR;
                            for(int i=0;i<operands[OP2].length();i++)
                                insert_machinecode(Instset.CQW.ordinal(),extractNumber(operands[OP1])+i, extractNumber(""+((int)operands[OP2].charAt(i))), 0);
                            code_lineoffsets.put(code_currline+1,operands[OP2].length());
                        }
                        else throw new Exception("The instruction is malformed");
                            break;
                    case "POP":
                        if(op_types[0].equals("R")) insert_machinecode(Instset.POP.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;
                    case "PSH":
                        if(op_types[0].equals("R")) insert_machinecode(Instset.PSH.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;
                    case "CMT":
                        if(op_types[0].equals("QT") && op_types[1].equals("K"))
                            insert_machinecode(Instset.CMT.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception("The instruction is malformed");
                        break;
                    case "CMP":
                        if(op_types[0].equals("QT") && op_types[1].equals("K"))
                            insert_machinecode(Instset.CMP.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception("The instruction is malformed");
                        break;
                    case "CME": 
                        if(op_types[0].equals("R") && op_types[1].equals("R"))
                            insert_machinecode(Instset.CME.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception("The instruction is malformed");
                        break;    
                    case "SEF": 
                        if(op_types[0].equals("F") && op_types[1].equals("K"))
                            insert_machinecode(Instset.SEF.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception("The instruction is malformed");
                        break;    
                    case "GEF": 
                        if(op_types[0].equals("M") && op_types[1].equals("F"))
                            insert_machinecode(Instset.GEF.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), 0);
                        else throw new Exception("The instruction is malformed");
                        break;    
                    case "BES": 
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BES.ordinal(), extractNumber(operands[OP1]), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BLW": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BLW.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BLW.ordinal(),getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;    
                    case "BLE": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BLE.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BLW.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BEQ": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BEQ.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BEQ.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BGE": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BGE.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BGE.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BGR": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BGR.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BGR.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BDI": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BDI.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BDI.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BZE": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BZE.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BZE.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "BNZ": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.BNZ.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.BNZ.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "CALL": 
                        if(op_types[0].equals("L"))
                            insert_machinecode(Instset.CALL.ordinal(), getJumpOffset(extractNumber(""+code_labels.get(operands[OP1].replace("@","")))), 0, 0);
                        else
                        if(op_types[0].equals("K"))
                            insert_machinecode(Instset.CALL.ordinal(), getJumpOffset(extractNumber(operands[OP1])), 0, 0);
                        else throw new Exception("The instruction is malformed");
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
                        else throw new Exception("The instruction is malformed");
                        break;
                    case "ADD":
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.ADD.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if((op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")))
                                insert_machinecode(Instset.ADDRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.ADDRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP3]), extractNumber(operands[OP2]));
                                else
                                    throw new Exception("The instruction is malformed");
                        break;   
                    case "SUB": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M")) //M M M
                            insert_machinecode(Instset.SUB.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.SUBRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.SUBKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "MUL": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.MUL.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if((op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K"))) //  M M K
                                insert_machinecode(Instset.MULRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.MULRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP3]), extractNumber(operands[OP2]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "DIV": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.DIV.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.DIVRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.DIVKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "AND": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.AND.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.ANDRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.ANDKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "OR": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.OR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.ORRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.ORKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "NOR": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.NOR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.NORRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.NORKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "XOR": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.XOR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.XORRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.XORKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "NAN": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.NAN.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.NANRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.NANKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "NOT": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.NOT.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "SHL": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.SHL.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.SHLRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.SHLKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "SHR": 
                        for(int i=0;i<MAX_OPERAND_COUNT;i++) // USING VARIABLES TO CALCULATE
                            if(op_types[i].equals("V")){operands[OP1+i]="M"+code_variables.get(operands[OP1+i])[0];op_types[i]="M";}
                        if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("M"))
                            insert_machinecode(Instset.SHR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                        else
                            if(op_types[0].equals("M") && op_types[1].equals("M") && op_types[2].equals("K")) // M M K
                                insert_machinecode(Instset.SHRRK.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                            else
                                if(op_types[0].equals("M") && op_types[1].equals("K") && op_types[2].equals("M")) // M K M
                                    insert_machinecode(Instset.SHRKR.ordinal(), extractNumber(operands[OP1]), extractNumber(operands[OP2]), extractNumber(operands[OP3]));
                                else throw new Exception("The instruction is malformed");
                        break;   
                    case "INT":
                        if(op_types[0].equals("K")) insert_machinecode(Instset.INT.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "DLY": 
                        if(op_types[0].equals("K")) insert_machinecode(Instset.DLY.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "NOP": 
                        insert_machinecode(Instset.NOP.ordinal(), 0, 0, 0);
                        break;   
                    case "HLT": 
                        insert_machinecode(Instset.HLT.ordinal(), 0, 0, 0);
                        break;   
                    case "X": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.X.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "Y": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.Y.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "Z": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.Z.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "H": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.H.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "S": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.S.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "T": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.T.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "ROX": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.ROX.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "ROY": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.ROY.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "ROZ": 
                        if(op_types[0].equals("Q")) insert_machinecode(Instset.ROZ.ordinal(), extractNumber(operands[OP1]),0,0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "CNO": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.CNO.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "CSI": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.CSI.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "SWA": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.SWA.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "INC": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.INC.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "DEC": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.DEC.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "SWQ": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.SWQ.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "SWI": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q")) insert_machinecode(Instset.SWI.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),0);
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "CSW": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q") && op_types[2].equals("Q")) 
                            insert_machinecode(Instset.CSW.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),extractNumber(operands[OP3]));
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "TOF": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q") && op_types[2].equals("Q")) 
                            insert_machinecode(Instset.TOF.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),extractNumber(operands[OP3]));
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "DEU": 
                        if(op_types[0].equals("Q") && op_types[1].equals("Q") && op_types[2].equals("Q")) 
                            insert_machinecode(Instset.DEU.ordinal(), extractNumber(operands[OP1]),extractNumber(operands[OP2]),extractNumber(operands[OP3]));
                        else throw new Exception("The instruction is malformed");
                        break;   
                    case "": break; // IGNORE EMPTY LINE
                    default:
                        // USING VARIABLES (MAY OR MAY NOT IMPLEMENT)
                        break;
                }
            }
        }catch(Exception e){
            code_currline++;
            success=code_currline;
            System.err.println("There was an error in the line: "+code_currline+". "+e.getMessage());
            e.printStackTrace();
        }
        insert_machinecode(Instset.HLT.ordinal(), 0, 0, 0);
        insert_machinecode(Instset.NOP.ordinal(), 0, 0, 0);
        close_file();
        return success;
    }
}