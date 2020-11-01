##START##

.data

.text
.global F1_get_N
	F1_get_N:
#Prologo
	pushq %rbp
	movq %rsp, %rbp
	addq $~16, %rsp
#Cuerpo
	L1:
	#Guardando registros calleesaves\n
	#Fin de registros calleesaves\n
	#Cargando los argumentos\n
	movq %rbp, %rcx
	addq $~8, %rcx
	movq %rdi, (%rcx)
	#Fin de los argumentos\n
	movq %rbp, %rax
	addq $~8, %rax
	movq (%rax), %rax
	addq $~16, %rax
	movq (%rax), %rax
	#Restaurando registros calleesaves\n
	#Fin de registros calleesaves\n
	jmp L0
	L0:

#Epilogo
	movq %rbp, %rsp
	popq %rbp
	ret

.global F2_return_N
	F2_return_N:
#Prologo
	pushq %rbp
	movq %rsp, %rbp
	addq $~16, %rsp
#Cuerpo
	L3:
	#Guardando registros calleesaves\n
	#Fin de registros calleesaves\n
	#Cargando los argumentos\n
	movq %rbp, %rcx
	addq $~8, %rcx
	movq %rdi, (%rcx)
	#Fin de los argumentos\n
	movq %rbp, %rdi
	addq $16, %rdi
	movq (%rdi), %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	call F1_get_N
	#Restaurando registros calleesaves\n
	#Fin de registros calleesaves\n
	jmp L2
	L2:

#Epilogo
	movq %rbp, %rsp
	popq %rbp
	ret

.global _tigermain
	_tigermain:
#Prologo
	pushq %rbp
	movq %rsp, %rbp
	addq $~16, %rsp
#Cuerpo
	L5:
	#Cargando los argumentos\n
	movq %rbp, %rcx
	addq $~8, %rcx
	movq %rdi, (%rcx)
	#Fin de los argumentos\n
	movq %rbp, %rax
	addq $~16, %rax
	movq $8, %r9
	movq %r9, (%rax)
	movq %rbp, %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	call F2_return_N
	jmp L4
	L4:

#Epilogo
	movq %rbp, %rsp
	popq %rbp
	ret

##FINISH##
