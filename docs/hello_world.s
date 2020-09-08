.data
	fmt: .asciz "%d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n"
	a: .quad 1
	b: .quad 2
	c: .quad 3
	d: .quad 4
	e: .quad 5
	f: .quad 6
	g: .quad 7
	h: .quad 8
	i: .quad 9
	j: .quad 10

.global main
.text
	main:
		xorq  %rax, %rax # cantidad de valores de punto flotante
		movq  $fmt, %rdi # el primer argumento es el formato		
		movq  a,    %rsi # segundo argumento
		movq  b,    %rdx
		movq  c,    %rcx
		movq  d,    %r8
		movq  e,    %r9
		pushq  j
		pushq  i
		pushq  h
		pushq  g
		pushq  f
		call printf
		popq %rax
		popq %rax
		popq %rax
		popq %rax
		popq %rax		
		ret

#gcc -o output -fno-asynchronous-unwind-tables -no-pie hello_world.s
