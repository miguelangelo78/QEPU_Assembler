jmp @main
#SOURCE BEGIN:stdio.qasm
#STANDARD INPUT OUT:
@std_out_char
	int 0 
ret
@std_out_buff
	int 1
ret
@std_in_noecho_char
	int 2
ret
@std_in_char
	int 3
ret
@std_in_noecho_buff
	int 4
ret
@std_in_buff
	int 5
ret
#SOURCE END: stdio.qasm
#SOURCE BEGIN:stdlib.qasm
#CONVERTING DATA:
@std_ctoi
	sub r0 r0 48
ret
@std_itoc
	add r0 r0 48
ret
@std_itos
	
ret
#LOOPS:
@for_loop
	mov r2 0       #DATA 1
	mov r3 10      #DATA 2
	@for_ret  	   #FOR LOOP RETURN
	cme r2 r3 	   #COMPARE
	bge @for_exit  #EXIT IF R2>R3
		#DO STUFF HERE
	add r2 r2 1    #INCREMENT R2
	jmp @for_ret   #RETURN TO BEGINNING OF LOOP
	@for_exit      #EXIT FOR LOOP
ret
@while_loop


ret
#SOURCE END: stdlib.qasm


@main

mov r3 10
mov r4 10
mov m0 "Teste"
mov roc *m0
int 7
gea 0