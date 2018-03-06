.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rcx
    xorq 	$1, %rcx
    cmpq 	$1, %rcx
    je	 	then0
    movq 	$0, %rcx
    jmp	 	if_end0
then0:
    movq 	$1, %rcx
if_end0:
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_bool
    movq 	$0, %rax
    leaveq
    retq
