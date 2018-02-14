    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$16, %rsp
    callq 	_read_int
    movq 	%rax, -16(%rbp)
    movq 	-16(%rbp), %rax
    leaveq
    retq
