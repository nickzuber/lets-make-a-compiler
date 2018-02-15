    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	%rcx, %rsi
    addq 	%rdx, %rsi
    movq 	%rsi, %rax
    leaveq
    retq
