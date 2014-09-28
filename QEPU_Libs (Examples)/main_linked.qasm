jmp @main
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
get stdio.qep

@main

mov r3 10
mov r4 10
mov m0 "Teste"
mov roc *m0
int 7