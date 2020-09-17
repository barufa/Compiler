##START##

##Hola mundo coloreado a mano

.data
L0:	.quad 11
	.string "Hola Mundo\n"

.text
.global _tigermain
	_tigermain:
#Prologo
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
#Cuerpo
	L2:
	
	#Cargando los argumentos\n
	
	movq %rbp, %r15
	movq $16, %r14
	addq %r14, %r15
	movq %rdi, (%r15)
	
	leaq L0(%rip), %r15
	movq %r15, %r14
	
	xorq %rax, %rax #cnt argumentos de punto flotante
	movq %r14, %rdi
	call print
	
	movq $0, %r15
	movq %r15, %rax
	
	jmp L1
	
	L1:
	
#Epilogo
	movq %rbp, %rsp
	popq %rbp
	ret

##FINISH##
