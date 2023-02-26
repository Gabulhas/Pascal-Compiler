	.text
	.globl	main
main:
	movq %rsp, %rbp
	call while_test
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
while_test:
	pushq %rsp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $0, %rdi
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
while_1START:
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	pushq %rdi
	movq $10, %rdi
	popq %rsi
	cmpq %rdi, %rsi
	setl %dil
	movzbq %dil, %rdi
	movq $1, %rsi
	cmpq %rdi, %rsi
	jne while_1END
#PRINT
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	call print_int
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	pushq %rdi
	movq $1, %rdi
	popq %rsi
	addq %rsi, %rdi
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
	jmp while_1START
while_1END:
	addq $8, %rsp
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
.SScan_int:
	.string "%d"
