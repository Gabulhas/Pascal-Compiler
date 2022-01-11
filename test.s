	.text
	.globl	main
main:
	movq %rsp, %rbp
	call read_val
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
scan_int:
	subq $8, %rsp
	xorl %eax, %eax
	movq $.SScan_int, %rdi
	movq %rsp, %rsi
	call scanf
	popq %rdi
	ret
read_val:
	pushq %rsp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rbp, %rsi
	pushq %rsi
	call scan_int
	popq %rsi
	movq %rdi, -8(%rsi)
#PRINT
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	call print_int
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	pushq %rdi
	movq $10, %rdi
	popq %rsi
	cmpq %rdi, %rsi
	setg %dil
	movzbq %dil, %rdi
	movq $1, %rsi
	cmpq %rdi, %rsi
	jne IF_1_else
	movq $1, %rdi
	movq %rbp, %rsi
	movq %rdi, -16(%rsi)
	jmp IF_1_END
IF_1_else:
	movq $0, %rdi
	movq %rbp, %rsi
	movq %rdi, -16(%rsi)
IF_1_END:
	movq %rbp, %rsi
	movq -16(%rsi), %rdi
	movq $1, %rsi
	cmpq %rdi, %rsi
	jne IF_2_else
#PRINT
	movq $1, %rdi
	call print_int
	jmp IF_2_END
IF_2_else:
#PRINT
	movq $0, %rdi
	call print_int
IF_2_END:
	addq $16, %rsp
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
.SScan_int:
	.string "%d"
