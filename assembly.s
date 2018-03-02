    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	%rdx, %r8
    addq 	%rcx, %r8
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	%rdx, %rdx
    addq 	%rcx, %rdx
    movq 	%r8, %rcx
    addq 	%rdx, %rcx
    movq 	%rcx, %rax
    leaveq
    retq
