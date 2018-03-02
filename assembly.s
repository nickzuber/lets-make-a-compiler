    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$11, %rcx
    movq 	%rcx, %rcx
    movq 	$1, %rdx
    negq 	%rdx
    movq 	%rdx, %rdx
    addq 	%rcx, %rdx
    movq 	%rdx, %rax
    leaveq
    retq
