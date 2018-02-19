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
    movq 	$1, %rdi
    addq 	$1, %rdi
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	%rdi, %r9
    addq 	%r8, %r9
    movq 	%rsi, %r10
    addq 	%r9, %r10
    movq 	%r10, %rax
    leaveq
    retq
