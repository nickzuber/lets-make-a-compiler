.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$0, %rcx
    xorq 	$1, %rcx
    cmpq 	$1, %rcx
    je	 	then0
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
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	%r8, %r9
    addq 	%rcx, %r9
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	%r8, %r8
    addq 	%rcx, %r8
    movq 	%r9, %rcx
    addq 	%r8, %rcx
    movq 	%rdx, %rdx
    addq 	%rcx, %rdx
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	%r8, %r8
    addq 	%rcx, %r8
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	$1, %r9
    addq 	$1, %r9
    movq 	%rcx, %rcx
    addq 	%r9, %rcx
    movq 	%r8, %r9
    addq 	%rcx, %r9
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	%r8, %r8
    addq 	%rcx, %r8
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	$1, %r10
    addq 	$1, %r10
    movq 	%rcx, %rcx
    addq 	%r10, %rcx
    movq 	%r8, %r8
    addq 	%rcx, %r8
    movq 	%r9, %rcx
    addq 	%r8, %rcx
    movq 	%rdx, %rdx
    addq 	%rcx, %rdx
    movq 	%rdx, %rcx
    jmp	 	if_end0
then0:
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	%rcx, %rcx
    addq 	%rdx, %rcx
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	%rdx, %rdx
    addq 	%r8, %rdx
    movq 	%rcx, %rcx
    addq 	%rdx, %rcx
    movq 	%rcx, %rcx
if_end0:
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_int
    movq 	$0, %rax
    leaveq
    retq
