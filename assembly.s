    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$32, %rsp
    callq 	_read_int
    movq 	%rax, -16(%rbp)
    callq 	_read_int
    movq 	%rax, -24(%rbp)
    movq 	-16(%rbp), %rax
    movq 	%rax, -32(%rbp)
    movq 	-24(%rbp), %rax
    addq 	%rax, -32(%rbp)
    movq 	-32(%rbp), %rax
    leaveq
    retq
