##START##

.data
L3:	.quad 1
	.string "\n"
spill1:	.quad 0
spill2:	.quad 0
spill3:	.quad 0
spill4:	.quad 0
spill5:	.quad 0
spill6:	.quad 0
spill7:	.quad 0

.text
.global fact
	fact:
#Prologo
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
#Cuerpo
	L5:
	#Guardando registros calleesaves\n
	pushq %rbx
	pushq %r10
	pushq %r11
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	#Cargando los argumentos\n
	movq %rbp, %rdx
	movq $16, %rcx
	addq %rcx, %rdx
	movq %rdi, (%rdx)
	movq %rsi, %rsi
	movq $0, %rax
	cmpq %rax, %rsi
	je L0
	L1:
	movq %rsi, %rbx
	movq %rsi, %rsi
	movq $1, %rax
	subq %rax, %rsi
	movq %rsi, %rsi
	movq %rbp, %rdi
	movq $16, %rcx
	addq %rcx, %rdi
	movq (%rdi), %rdi
	movq %rdi, %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	movq %rdi, %rdi
	movq %rsi, %rsi
	call fact
	movq %rax, %rax
	movq %rbx, %rbx
	imulq %rax, %rbx
	movq %rbx, %rbx
	L2:
	movq %rbx, %rax
	#Restaurando registros calleesaves\n
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %r11	
	popq %r10	
	popq %rbx	
	jmp L4
	L0:
	movq $1, %rbx
	movq %rbx, %rbx
	jmp L2
	L4:
	


#Epilogo
	movq %rbp, %rsp
	popq %rbp
	ret

.global _tigermain
	_tigermain:
#Prologo
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
#Cuerpo
	L7:
	#Cargando los argumentos\n
	movq %rbp, %rcx
	movq $16, %rbx
	addq %rbx, %rcx
	movq %rdi, (%rcx)
	movq $4, %rsi
	movq %rsi, %rsi
	movq %rbp, %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	movq %rdi, %rdi
	movq %rsi, %rsi
	call fact
	movq %rax, %rdi
	movq %rdi, %rdi
	movq $64, %rcx
	addq %rcx, %rdi
	movq %rdi, %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	movq %rdi, %rdi
	call chr
	movq %rax, %rdi
	movq %rdi, %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	movq %rdi, %rdi
	call print
	leaq L3(%rip), %rdi
	movq %rdi, %rdi
	xorq %rax, %rax #cnt argumentos de punto flotante
	movq %rdi, %rdi
	call print
	movq $0, %rax
	movq %rax, %rax
	jmp L6
	L6:
	


#Epilogo
	movq %rbp, %rsp
	popq %rbp
	ret

##FINISH##

