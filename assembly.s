    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$32, %rsp
    movq 	$1, -32(%rbp)
    callq 	_read_int
    movq 	%rax, -8(%rbp)
    callq 	_read_int
    movq 	%rax, -16(%rbp)
    movq 	-8(%rbp), %rax
    movq 	%rax, -24(%rbp)
    movq 	-16(%rbp), %rax
    addq 	%rax, -24(%rbp)
    movq 	-24(%rbp), %rax
    leaveq
    retq
