.globl _main

_main:  
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
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	%rdx, %rdx
    addq 	%r8, %rdx
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	$1, %r9
    addq 	$1, %r9
    movq 	%r8, %r8
    addq 	%r9, %r8
    movq 	%rdx, %rdx
    addq 	%r8, %rdx
    movq 	%rcx, %rcx
    addq 	%rdx, %rcx
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_int
    movq 	$0, %rax
    leaveq
    retq
