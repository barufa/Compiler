.data
	fmt: .asciz "%d" #"Hola Mundo\n"
	i: .quad 0

.global main
.text
	main:
		movq $fmt, %rdi # el primer argumento es el formato
		movq i, %rsi # el valor a imprimir
		xorq %rax, %rax # cantidad de valores de punto flotante
		call printf
		ret

#gcc -o output -fno-asynchronous-unwind-tables -no-pie hello_world.s 
