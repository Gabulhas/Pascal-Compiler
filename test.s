	.text
	.globl	main
main:
	movq %rsp, %rbp
	call for_test
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
for_test:
	pushq %rsp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rbp, %rsi
	pushq %rsi
	call scan_int
	popq %rsi
	movq %rdi, -16(%rsi)
#PRINT
	movq %rbp, %rsi
	movq -16(%rsi), %rdi
	call print_int
	movq $10, %rdi
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
while_1START:
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	pushq %rdi
	movq %rbp, %rsi
	movq -16(%rsi), %rdi
	popq %rsi
	cmpq %rdi, %rsi
	setle %dil
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
	movq %rbp, %rsi
	movq -16(%rsi), %rdi
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
while_2START:
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	pushq %rdi
	movq $10, %rdi
	popq %rsi
	cmpq %rdi, %rsi
	setge %dil
	movzbq %dil, %rdi
	movq $1, %rsi
	cmpq %rdi, %rsi
	jne while_2END
#PRINT
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	call print_int
	movq $1, %rdi
	pushq %rdi
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	popq %rsi
	subq %rsi, %rdi
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
	jmp while_2START
while_2END:
	movq $10, %rdi
	movq %rbp, %rsi
	movq %rdi, -8(%rsi)
while_3START:
	movq %rbp, %rsi
	movq -8(%rsi), %rdi
	pushq %rdi
	movq %rbp, %rsi
	movq -16(%rsi), %rdi
	popq %rsi
	cmpq %rdi, %rsi
	setle %dil
	movzbq %dil, %rdi
	movq $1, %rsi
	cmpq %rdi, %rsi
	jne while_3END
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
	jmp while_3START
while_3END:
	addq $16, %rsp
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
.SScan_int:
	.string "%d"
