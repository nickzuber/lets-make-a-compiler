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
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	%rcx, %rcx
    addq 	%rdx, %rcx
    movq 	%r8, %rdx
    addq 	%rcx, %rdx
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%r8
    pushq 	%r9
    pushq 	%r10
    pushq 	%r11
    callq 	_read_int
    popq 	%r11
    popq 	%r10
    popq 	%r9
    popq 	%r8
    popq 	%rdx
    popq 	%rcx
    movq 	%rax, %rcx
    movq 	%rdx, %rdx
    addq 	%rcx, %rdx
    movq 	%rdx, %rax
    leaveq
    retq
