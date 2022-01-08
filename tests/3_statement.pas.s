.text
	.globl	main
main:
	movq %rsp, %rbp
	call statement
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
statement:
	pushq %rsp
	movq %rsp, %rbp
	subq $48, %rsp
	pushq $4
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
	.data
.Sprint_int:
	.string "%d\n"
