    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    callq 	_read_int
    movq 	%rax, %rcx
    movq 	%rcx, %rdx
    addq 	$1, %rdx
    movq 	%rdx, %rax
    leaveq
    retq
