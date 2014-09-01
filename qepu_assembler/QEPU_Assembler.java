package qepu_assembler;

import javax.swing.JFrame;

public class QEPU_Assembler {
    public static void main(String[] args) {
        Window window=new Window();
        window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        window.setTitle("Quantum Electronic Processing Unit Assembler");
        window.setLocationRelativeTo(null);
        window.setVisible(true);
    }
}