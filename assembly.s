.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rax
    cmpq 	$1, %rax
    je	 	then0
    movq 	$2, %rcx
    jmp	 	if_end0
then0:
    movq 	$1, %rcx
    movq 	$2, %rcx
    movq 	$3, %rcx
    movq 	$4, %rcx
    movq 	$5, %rcx
    movq 	$6, %rcx
    movq 	$7, %rcx
    movq 	$8, %rcx
if_end0:
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_int
    movq 	$0, %rax
    leaveq
    retq
